(ns dk.cst.corpus-probe.cwb
  "Child-process driver for CQP, the query processor of the IMS Open
  Corpus Workbench: one batch of commands per corpus under a deadline,
  its output split into sections and its errors scrubbed for display.

  The helpers are cwb.parse (the output), cwb.registry (the registry),
  cwb.corpus (the facts of a corpus), cwb.tools (the other CWB programs)
  and cwb.command (the commands)."
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
  "Return the stdin string sending `commands` to a child-mode CQP process,
  each followed by `eol-command`."
  [commands]
  ;; the marker on a line of its own, or a trailing `#` comment swallows it
  (str (str/join "\n" (interleave commands (repeat eol-command))) "\n"))

(defn stdout->sections
  "Split raw child-mode stdout `s` into the version banner and per-command
  output sections: {:banner <line or nil> :sections [[line ...] ...]}, one
  section per `eol-marker`."
  [s]
  ;; newline only: corpus data may hold every other control character
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
  cwb-* tools print on startup whatever the batch: an entry whose ID field
  does not match its filename, or a file in the registry directory that is
  no entry. They name absolute server paths."
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
  "Prepare CQP `error` for display: its message without the registry
  diagnostics (see `diagnostic?`), which may name server paths, and without
  the follow-on errors of a failed query; the query error text itself stays
  verbatim."
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
  been destroyed. `opts` may set :in (text for stdin), :env (extra
  environment variables) and :charset (of all three streams, default
  UTF-8)."
  [cmd timeout-ms {:keys [in env charset] :or {charset "UTF-8"}}]
  (let [proc (p/process (cond-> {:cmd cmd :shutdown p/destroy-tree}
                          in  (assoc :in (io/input-stream
                                          (.getBytes ^String in charset)))
                          env (assoc :extra-env env)))
        ;; both streams drained at once, or a process filling one of them
        ;; deadlocks on the other
        out  (future (slurp (:out proc) :encoding charset))
        err  (future (slurp (:err proc) :encoding charset))
        res  (deref proc timeout-ms ::timeout)]
    (if (= res ::timeout)
      (do (p/destroy-tree proc)
          {:timeout? true :timeout-ms timeout-ms})
      {:out @out :err @err :exit (:exit res)})))

(defn timeout?
  "True when `x` reports a process killed for taking too long: the result
  of `run!` or of `run-batch!`, or an exception thrown for one (see
  `batch!`)."
  [x]
  (boolean (if (instance? Throwable x)
             (= :timeout (get-in (ex-data x) [:error :type]))
             (or (:timeout? x)
                 (= :timeout (get-in x [:error :type]))))))

(defn run-batch!
  "Run CQP `commands` as one child-mode batch against `ctx`: {:banner ...
  :results [[line ...] ...] :warnings [...] :error ... :exit ...}, the
  :results aligned with `commands`, the :warnings the registry
  diagnostics, the :error nil or a map of :type :timeout, :cqp (its
  :message the stderr text) or :misaligned (fewer or more sections than
  commands, so the results cannot be trusted).

  The :registry of `ctx` must be an absolute path."
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
            ;; stderr beyond the diagnostics is the one sign that a command
            ;; failed; the exit code says nothing
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
  `ctx` (see `run-batch!`); throws ex-info with the :error, the :corpus and
  the `query` they ran when CQP reports an error, times out or dies."
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

(def rejection-keys
  "What a guard's ex-data may tell the reader: why it refused and the
  attributes it named. Selected rather than merged, since the ex-data of
  the tool guards carries a command line and a registry path."
  [:reason :attr :attrs])

(defn error-map
  "The error map for exception `e` thrown by a search: the CQP error it
  carries, a :rejected error with the message and the `rejection-keys` of
  one of this project's own guards (an ex-info without one), or :internal
  for anything else, logged rather than shown since its message may name
  a server path.

  The corpus is left out: the view groups the corpora that failed the
  same way, which two error maps differing only by corpus would split."
  [e]
  (or (:error (ex-data e))
      (if (instance? clojure.lang.ExceptionInfo e)
        (merge {:type :rejected :message (ex-message e)}
               (select-keys (ex-data e) rejection-keys))
        (do (t/error! ::internal-error e)
            {:type :internal}))))

(defn attempt
  "Call no-arg `f` for `corpus` without failing: what it returns, or
  {:corpus ... :error ...} with the `error-map` of what it threw. Given
  `g`, a function of the exception, its map joins the failure's."
  ([corpus f]
   (attempt corpus f (constantly nil)))
  ([corpus f g]
   (try (f)
        (catch Exception e
          (merge {:corpus corpus :error (error-map e)} (g e))))))

(defn locale
  "The java.util.Locale named by LC_ALL value `s` (\"da_DK.UTF-8\"), or the
  root locale when it names none."
  [s]
  ;; a POSIX locale name is a BCP 47 tag with an underscore for the hyphen
  ;; and a charset or modifier suffix
  (-> (str s)
      (str/replace #"[.@].*$" "")
      (str/replace "_" "-")
      (Locale/forLanguageTag)))

(defn ->collator
  "A collator over annotation values in the locale `ctx` sorts in (its
  :sort-locale, see `locale`): the one CQP sorts a concordance in, so a
  value list and a concordance agree on where æ, ø and å belong."
  [ctx]
  ;; a new one per caller: collators are stateful
  (Collator/getInstance (locale (:sort-locale ctx))))

(defn pmap-n
  "Map `f` over `coll` with at most `n` calls running at once, in order."
  [n f coll]
  ;; pmap alone starts a chunk of 32 at once, and each call spawns a process
  (mapcat #(doall (pmap f %)) (partition-all n coll)))

(defn parallelism
  "How many corpora `ctx` queries at once (its :parallelism, default 8)."
  [ctx]
  (:parallelism ctx 8))

(defn deadline
  "The wall-clock deadline (a millisecond timestamp) of a search started
  now under `ctx`: its :search-budget-ms (default 60000) from now, after
  which no further corpus is queried."
  [{:keys [search-budget-ms] :or {search-budget-ms 60000} :as ctx}]
  (+ (System/currentTimeMillis) search-budget-ms))

(defn overdue?
  "True once `deadline` (see `deadline`) has passed."
  [deadline]
  (> (System/currentTimeMillis) deadline))

(defn within-deadline
  "Cut the timeouts of `ctx` down to the time left before `deadline`:
  the budget alone bounds how many corpora a search starts, not how long
  the last one may run."
  [ctx deadline]
  (let [left (max 1000 (- deadline (System/currentTimeMillis)))]
    (cond-> ctx
      (:timeout-ms ctx)       (update :timeout-ms min left)
      (:query-timeout-ms ctx) (update :query-timeout-ms min left))))

(defn running-ctx
  "Give `ctx` the longer timeout a batch that runs the query needs (its
  :query-timeout-ms), leaving batches that only read a saved result on the
  ordinary :timeout-ms. Counting and showing run the same query and get
  the same budget, or a corpus could time out while counted and succeed
  while shown."
  [ctx]
  (cond-> ctx
    (:query-timeout-ms ctx) (assoc :timeout-ms (:query-timeout-ms ctx))))
