(ns dk.cst.corpus-probe.cqp
  "Child-process driver for CQP, the query processor of the IMS Open Corpus
  Workbench.

  CQP is spawned per batch as `cqp -c -r <registry>` (child mode). Commands
  are written to stdin, each followed by the pseudo-command `.EOL.;`, and
  stdout is split into per-command sections on the `-::-EOL-::-` marker line
  CQP prints in response; the marker arrives even after errors, so sections
  always align with commands. Any stderr output beyond the registry
  diagnostics CQP prints on startup means some command in the batch failed;
  the exit code is meaningless. See PLAN.md §5 and
  docs/research/cqp-integration.md for the verified protocol."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [babashka.process :as p]))

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

(defn run-process!
  "Run command vector `cmd` to completion and return {:out :err :exit}, or
  ::timeout once `timeout-ms` have passed and the process tree has been
  destroyed.

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
          ::timeout)
      {:out @out :err @err :exit (:exit res)})))

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
  (let [res (run-process! [cqp "-c" "-r" registry]
                          timeout-ms
                          (cond-> {:in      (commands->stdin commands)
                                   :charset charset}
                            ;; LC_ALL sets the collation CQP's ExternalSort uses
                            sort-locale (assoc :env {"LC_ALL" sort-locale})))]
    (if (= res ::timeout)
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

(defn version!
  "Return the CQP version banner reported by the `ctx` installation."
  [ctx]
  (:banner (run-batch! ctx [])))
