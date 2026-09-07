(ns dk.cst.corpus-probe.test.cwb
  "The integration gate and the fixtures of the tests that run CWB: a
  context over the dev registry, temporary registries and contexts over
  them, the golden files of captured output, and the collator of a Danish
  installation. Every test namespace takes these from here rather than
  from another test namespace."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [is]]
            [dk.cst.corpus-probe.cache :as cache]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.tools :as tools]))

(def ctx
  "A context over the dev registry, encoded by dev/encode.sh."
  {:registry (str (System/getProperty "user.dir") "/dev/corpus/registry")})

(def cwb-ready?
  "True when the cqp binary and all encoded dev corpora are present; the
  integration tests are skipped otherwise (see `when-cwb`)."
  (boolean (and (fs/which "cqp")
                (every? #(.exists (io/file (:registry ctx) %))
                        ["probe" "viser" "taler"]))))

(defmacro when-cwb
  "Run integration test `body` when CWB is available, else pass trivially."
  [& body]
  `(if cwb-ready?
     (do ~@body)
     (is true "skipped: cqp or dev corpus missing (run dev/encode.sh)")))

(def fixture
  "The registry entry fixture that needs no encoded corpus."
  "test/resources/registry-probe")

(defn golden-lines
  "The lines of golden file `filename` under test/resources/golden/, the
  byte-exact output captured by dev/capture-golden.sh."
  [filename]
  (str/split (slurp (str "test/resources/golden/" filename)) #"\n"))

(defn temp-registry!
  "A fresh registry directory (as a path) holding `source` (a registry
  entry file) as the entry `probe` plus `extras`, a map of filename to
  content."
  [source extras]
  (let [reg (fs/create-temp-dir)]
    (fs/copy source (fs/file reg "probe"))
    (doseq [[name content] extras]
      (spit (fs/file reg name) content))
    (str reg)))

(defn mismatched-entry
  "The text of registry entry file `source` with its ID field changed, an
  entry whose ID does not match its filename."
  [source]
  (str/replace (slurp source) #"(?m)^ID .*" "ID   mismatch"))

(defn encode!
  "A ctx over a fresh registry whose entry `probe` points at a fresh home
  holding the token stream `data` of its one attribute; the home rides
  along as :home so a test can re-encode it, and `charset`, when given,
  is declared in the entry."
  ([data]
   (encode! data nil))
  ([data charset]
   (let [registry (fs/create-temp-dir)
         home     (fs/create-temp-dir)]
     (spit (fs/file registry "probe")
           (str "NAME \"\"\nID probe\nHOME " home "\nATTRIBUTE word\n"
                (when charset (str "##:: charset = \"" charset "\"\n"))))
     (spit (fs/file home "word.corpus") data)
     {:registry (str registry) :home home})))

(defn cache-ctx!
  "A context over a fresh cache directory and a registry holding an entry
  for PROBE and one for VISER, so that `build-stamp` has something to read
  for each and two corpora differ by more than a missing entry."
  []
  (let [registry (fs/create-temp-dir)]
    (doseq [id ["probe" "viser"]]
      (spit (fs/file registry id)
            (str "NAME \"\"\nID " id "\nHOME /nowhere\nATTRIBUTE word\n")))
    {:registry  (str registry)
     :cache-dir (str (fs/create-temp-dir))}))

(defn phantom-ctx!
  "A context over a fresh registry holding one entry whose HOME points at
  data that is not there, which is what the three phantom KU entries look
  like.

  Fresh per call: the per-corpus facts cache is keyed by the registry path
  and the entry's mtime, so two checks of one registry would share the
  first one's outcome."
  []
  {:registry (temp-registry! fixture {})})

(defn reset-cache!
  "A fixture running test `f` with the process-wide state of the cache
  reset before and after it: a remembered count would otherwise outlive
  the corpus it was counted from, a test that moves the reaping timestamp
  would silence the reaping of every test after it, and a search left in
  flight would answer the next one's question."
  [f]
  (cache/forget-counts!)
  (reset! cache/last-reap 0)
  (reset! cache/in-flight {})
  (f)
  (cache/forget-counts!)
  (reset! cache/last-reap 0)
  (reset! cache/in-flight {}))

(defmacro with-value-limit
  "Run `body` with dk.cst.corpus-probe.cwb.tools/value-limit bound to `n`
  and the facts cache emptied before and after: the value lists decoded
  under one limit are cached, and would be served under another."
  [n & body]
  `(with-redefs [tools/value-limit ~n]
     (reset! corpus/facts-cache {})
     (try ~@body (finally (reset! corpus/facts-cache {})))))

(def da-collator
  "The collator of a Danish installation, as the handlers build it."
  (delay (cwb/->collator {:sort-locale "da_DK.UTF-8"})))
