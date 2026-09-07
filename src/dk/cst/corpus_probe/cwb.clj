(ns dk.cst.corpus-probe.cwb
  "Child-process driver for CQP, the query processor of the IMS Open Corpus
  Workbench, and what running it for a request needs beside it: the error
  text scrubbed for display, the collation the installation sorts in, and
  running one batch per corpus under a deadline. Its helpers parse the
  output (dk.cst.corpus-probe.cwb.parse), read the registry
  (dk.cst.corpus-probe.cwb.registry), hold the facts of a corpus
  (dk.cst.corpus-probe.cwb.corpus), run the other CWB programs
  (dk.cst.corpus-probe.cwb.tools) and write the commands
  (dk.cst.corpus-probe.cwb.command).

  CQP is spawned per batch as `cqp -c -r <registry>` (child mode). Commands
  are written to stdin, each followed by the pseudo-command `.EOL.;`, and
  stdout is split into per-command sections on the `-::-EOL-::-` marker line
  CQP prints in response; the marker arrives even after errors, so sections
  always align with commands. Any stderr output beyond the registry
  diagnostics CQP prints on startup means some command in the batch failed;
  the exit code is meaningless. See PLAN.md §5 and
  docs/research/cqp-integration.md for the verified protocol."
  (:refer-clojure :exclude [run!])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [babashka.process :as p]
            [taoensso.telemere :as t])
  (:import [java.text Collator]
           [java.util Locale]))

(def eol-command
  "Pseudo-command making CQP echo `eol-marker`; sent after every command."
  ".EOL.;")

(def eol-marker
  "The line CQP prints in response to `eol-command`."
  "-::-EOL-::-")

(def progress-marker
  "Prefix of the TAB-separated progress lines emitted under ProgressBar."
  "-::-PROGRESS-::-")

(defn commands->stdin
  "Return the stdin string sending `commands` to a child-mode CQP process.

  Each command (a string of one or more `;`-terminated CQP commands) is
  followed by `eol-command` on its own line, so that a trailing `#` comment
  in a command cannot swallow the marker."
  [commands]
  (str (str/join "\n" (interleave commands (repeat eol-command))) "\n"))

(defn stdout->sections
  "Split raw child-mode stdout `s` into the version banner and per-command
  output sections.

  Returns {:banner <line or nil> :sections [[line ...] ...]} with one section
  per `eol-marker` encountered. Splits on newline only, since corpus data
  may legally contain every other control character, and filters progress
  lines. Output after the final marker (normally just the trailing newline)
  is discarded."
  [s]
  (let [lines  (str/split s #"\n" -1)
        banner (when (re-find #"^CQP\s.*\d+\.\d+" (first lines))
                 (first lines))
        close  (fn [{:keys [current] :as acc}]
                 (-> acc
                     (update :sections conj current)
                     (assoc :current [])))]
    (->> (cond-> lines banner rest)
         (remove #(str/starts-with? % progress-marker))
         (reduce (fn [acc line]
                   (if (= line eol-marker)
                     (close acc)
                     (update acc :current conj line)))
                 {:sections [] :current []})
         :sections
         (assoc {:banner banner} :sections))))

(defn diagnostic?
  "True when stderr `line` is one of the registry diagnostics CQP and the
  cwb-* tools print on startup whatever the batch: a warning that an
  entry's ID field does not match its filename, or an error parsing a file
  of the registry directory that is not an entry. They concern the
  installation, not the commands, and name absolute server paths."
  [line]
  (or (str/starts-with? line "CL warning:")
      (str/starts-with? line "REGISTRY ERROR")))

(defn stderr->outcome
  "Split stderr text `s` into the registry diagnostics and the real error
  text: {:warnings [line ...] :error <text or nil>}."
  [s]
  (let [{warnings true errors false} (group-by diagnostic? (str/split-lines s))
        error (str/trim (str/join "\n" errors))]
    {:warnings (vec warnings)
     :error    (not-empty error)}))

(def follow-on-errors
  "The errors CQP adds for the later commands of a batch once an earlier
  one has failed: a query without an activated corpus, and every command
  on the then undefined `Last` or the metadata filter's subcorpus."
  ["CQP Error:\n\tNo corpus activated"
   "CQP Error:\n\tCorpus ``Last'' is undefined"
   "CQP Error:\n\tCorpus ``Filter'' is undefined"])

(defn drop-follow-on-errors
  "Remove the `follow-on-errors` from CQP stderr text `message` when it
  reports anything else, so the user sees the failing command's own error;
  a message of nothing but follow-on errors is kept as it is."
  [message]
  (let [trimmed (-> (reduce #(str/replace %1 %2 "") message follow-on-errors)
                    (str/replace #"\n{2,}" "\n")
                    (str/trim))]
    (if (str/blank? trimmed) message trimmed)))

(defn public-error
  "Prepare CQP `error` for display: drop the registry diagnostics from its
  message (see `diagnostic?`), which concern the server installation
  rather than the query and may name absolute server paths (never to
  reach a rendered page), and the follow-on errors our own batch commands
  add after a failed query. The query error text itself stays verbatim,
  `<--` pointer included."
  [error]
  (if-let [message (:message error)]
    (assoc error :message (->> (str/split message #"\n")
                               (remove diagnostic?)
                               (str/join "\n")
                               (drop-follow-on-errors)
                               (not-empty)))
    error))

(defn run!
  "Run command vector `cmd` to completion and return {:out :err :exit}, or
  {:timeout? true} once `timeout-ms` have passed and the process tree has
  been destroyed.

  `opts` may set :in (text written to stdin), :env (extra environment
  variables) and :charset (the encoding of stdin and of both output
  streams, default UTF-8). Both streams are drained concurrently, so a
  process filling one of them cannot deadlock the other."
  [cmd timeout-ms {:keys [in env charset] :or {charset "UTF-8"}}]
  (let [proc (p/process (cond-> {:cmd cmd :shutdown p/destroy-tree}
                          in  (assoc :in (io/input-stream
                                          (.getBytes ^String in charset)))
                          env (assoc :extra-env env)))
        out  (future (slurp (:out proc) :encoding charset))
        err  (future (slurp (:err proc) :encoding charset))
        res  (deref proc timeout-ms ::timeout)]
    (if (= res ::timeout)
      (do (p/destroy-tree proc)
          {:timeout? true :timeout-ms timeout-ms})
      {:out @out :err @err :exit (:exit res)})))

(defn timeout?
  "True when `x` reports that a process was killed for taking too long
  rather than answering: the result of `run!` or of `run-batch!`, or an
  exception thrown for one (see `batch!`)."
  [x]
  (boolean (if (instance? Throwable x)
             (= :timeout (get-in (ex-data x) [:error :type]))
             (or (:timeout? x)
                 (= :timeout (get-in x [:error :type]))))))

(defn run-batch!
  "Run CQP `commands` as one child-mode batch against `ctx`, returning
  {:banner ... :results [[line ...] ...] :warnings [...] :error ... :exit
  ...}.

  `ctx` holds :registry (absolute path, required) and optionally :cqp
  (executable name/path), :timeout-ms, :charset (the corpus encoding used
  for both stdin and stdout) and :sort-locale (an LC_ALL value giving CQP's
  ExternalSort its collation). `:results` aligns positionally with
  `commands`; `:warnings` are the registry diagnostics (see `diagnostic?`),
  which do not fail the batch; `:error` is nil on success, or a map with
  :type :timeout (process killed), :type :cqp (:message holds the remaining
  stderr text) or :type :misaligned (section count differs from command
  count: the process died early, or output data collided with the section
  marker; either way positional alignment is lost and the results must not
  be trusted)."
  [{:keys [registry cqp timeout-ms charset sort-locale]
    :or   {cqp "cqp" timeout-ms 30000 charset "UTF-8"}}
   commands]
  (let [res (run! [cqp "-c" "-r" registry]
                  timeout-ms
                  (cond-> {:in      (commands->stdin commands)
                           :charset charset}
                    ;; LC_ALL sets the collation CQP's ExternalSort uses
                    sort-locale (assoc :env {"LC_ALL" sort-locale})))]
    (if (timeout? res)
      {:error {:type :timeout :timeout-ms timeout-ms}}
      (let [{:keys [banner sections]} (stdout->sections (:out res))
            {:keys [warnings error]}  (stderr->outcome (:err res))]
        (cond-> {:banner   banner
                 :results  sections
                 :warnings warnings
                 :exit     (:exit res)}
          error
          (assoc :error {:type :cqp :message error})

          (and (nil? error) (not= (count sections) (count commands)))
          (assoc :error {:type :misaligned
                         :expected (count commands)
                         :received (count sections)}))))))

(defn batch!
  "The output sections of `commands` run as one batch against `corpus` via
  `ctx` (see `run-batch!`), `query` being what they run, which the error
  names: throws ex-info carrying the :error map, the :corpus and the
  :query when CQP reports an error, times out or dies."
  [ctx corpus query commands]
  (let [{:keys [results error]} (run-batch! ctx commands)]
    (when error
      (throw (ex-info "CQP batch failed"
                      {:corpus corpus :query query :error error})))
    results))

(defn version!
  "Return the CQP version banner reported by the `ctx` installation."
  [ctx]
  (:banner (run-batch! ctx [])))

(defn error-map
  "The error map for exception `e` thrown by a search: the CQP error it
  carries; a :rejected error with the message of one of this project's own
  guards (an ex-info without a CQP error); or an :internal error for
  anything else, whose details are logged rather than shown, since an
  exception message may name a server path."
  [e]
  (or (:error (ex-data e))
      (if (instance? clojure.lang.ExceptionInfo e)
        {:type :rejected :message (ex-message e)}
        (do (t/error! ::internal-error e)
            {:type :internal}))))

(defn attempt
  "Call no-arg `f` for `corpus` without failing: what it returns, or
  {:corpus ... :error ...} with the `error-map` of what it threw, the
  shape one corpus's failure takes beside the others' results. Given `g`,
  a function of the exception, its map joins the failure's, for a caller
  that reads more from the exception than the error."
  ([corpus f]
   (attempt corpus f (constantly nil)))
  ([corpus f g]
   (try (f)
        (catch Exception e
          (merge {:corpus corpus :error (error-map e)} (g e))))))

(defn locale
  "The java.util.Locale named by LC_ALL value `s` (\"da_DK.UTF-8\"), or the
  root locale when it names none.

  A POSIX locale name is its BCP 47 tag with an underscore for the hyphen
  and a charset or modifier suffix, so dropping the suffix and putting the
  hyphen back is the whole conversion."
  [s]
  (-> (str s)
      (str/replace #"[.@].*$" "")
      (str/replace "_" "-")
      (Locale/forLanguageTag)))

(defn ->collator
  "A collator over annotation values in the locale `ctx` sorts in (its
  :sort-locale, see `locale`).

  The values come out of the corpora, and this is the locale CQP itself
  collates them in when it sorts a concordance, so a value list and a
  concordance agree on where æ, ø and å belong. Collators are stateful,
  so each caller gets its own."
  [ctx]
  (Collator/getInstance (locale (:sort-locale ctx))))

(defn pmap-n
  "Map `f` over `coll` with at most `n` calls running at once, in order.

  `pmap` alone starts a whole chunk of 32 futures at once, and each call
  here spawns a process, so the fan-out is bounded instead."
  [n f coll]
  (mapcat #(doall (pmap f %)) (partition-all n coll)))

(defn parallelism
  "How many corpora `ctx` queries at once (its :parallelism, default 8)."
  [ctx]
  (:parallelism ctx 8))

(defn deadline
  "The wall-clock deadline (a millisecond timestamp) of a search started
  now under `ctx`: its :search-budget-ms (default 60000) from now, after
  which no further corpus is queried, so a query that times out in every
  corpus cannot hold a request for the sum of all the timeouts."
  [{:keys [search-budget-ms] :or {search-budget-ms 60000} :as ctx}]
  (+ (System/currentTimeMillis) search-budget-ms))

(defn overdue?
  "True once `deadline` (see `deadline`) has passed."
  [deadline]
  (> (System/currentTimeMillis) deadline))

(defn within-deadline
  "`ctx` with its timeouts cut down to the time left before `deadline`.

  Without this, :search-budget-ms bounds only how many corpora a search
  starts, not how long the last one may run: one started just before the
  deadline would get a whole timeout of its own on top of the budget, and
  the retry a search makes without its cache another."
  [ctx deadline]
  (let [left (max 1000 (- deadline (System/currentTimeMillis)))]
    (cond-> ctx
      (:timeout-ms ctx)       (update :timeout-ms min left)
      (:query-timeout-ms ctx) (update :query-timeout-ms min left))))

(defn running-ctx
  "`ctx` with the longer timeout a batch that runs the query needs (its
  :query-timeout-ms), leaving batches that only read a result already
  saved on the ordinary :timeout-ms.

  The two are orders of magnitude apart, the figures being in
  resources/config.edn beside the settings. Counting and displaying run
  the same query, so they get the same budget: giving counting less would
  let a corpus time out while being counted and succeed while being shown,
  and a count that fails is left out of the total, quietly costing the
  search pages of hits it has."
  [ctx]
  (cond-> ctx
    (:query-timeout-ms ctx) (assoc :timeout-ms (:query-timeout-ms ctx))))
