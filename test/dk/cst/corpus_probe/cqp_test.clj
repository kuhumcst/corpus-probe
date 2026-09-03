(ns dk.cst.corpus-probe.cqp-test
  "Protocol tests against the captured child-mode session, plus live
  integration tests that run when cqp and the dev corpus are available."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cqp :as cqp]))

(deftest commands->stdin-test
  (is (= "PROBE;\n.EOL.;\nsize Last;\n.EOL.;\n"
         (cqp/commands->stdin ["PROBE;" "size Last;"]))))

(deftest stdout->sections-test
  (testing "captured session splits into banner + aligned sections"
    (let [{:keys [banner sections]}
          (cqp/stdout->sections (slurp "test/resources/golden/session-raw.stdout"))]
      (is (= "CQP version 3.5.0" banner))
      (is (= 5 (count sections)))
      (testing "silent commands yield empty sections, incl. the failed one"
        (is (= [[] [] ["5"] []] (vec (butlast sections)))))
      (is (= 2 (count (last sections))))))
  (testing "output without a banner"
    (is (= {:banner nil :sections [["x"]]}
           (cqp/stdout->sections "x\n-::-EOL-::-\n"))))
  (testing "progress lines are filtered"
    (is (= [["5"]]
           (:sections (cqp/stdout->sections
                       "-::-PROGRESS-::-\t1\t1\t 50% complete\n5\n-::-EOL-::-\n"))))))

(def ctx
  {:registry (str (System/getProperty "user.dir") "/dev/corpus/registry")})

(def cwb-ready?
  "True when the cqp binary and all encoded dev corpora are present; the
  integration tests below are skipped otherwise."
  (boolean (and (fs/which "cqp")
                (every? #(.exists (io/file (:registry ctx) %))
                        ["probe" "viser" "taler"]))))

(defn temp-registry
  "A fresh registry directory (as a path) holding `source` (a registry
  entry file) as the entry `probe` plus `extras`, a map of filename to
  content; a test fixture shared by the registry-handling tests."
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

(defmacro when-cwb
  "Run integration test `body` when CWB is available, else pass trivially."
  [& body]
  `(if cwb-ready?
     (do ~@body)
     (is true "skipped: cqp or dev corpus missing (run dev/encode.sh)")))

(deftest version-integration-test
  (when-cwb
   (is (str/starts-with? (cqp/version! ctx) "CQP version"))))

(deftest run-batch-integration-test
  (when-cwb
   (testing "sections align with commands even when one fails"
     (let [{:keys [results error exit]}
           (cqp/run-batch! ctx ["PROBE;" "bogus;" "size Last;"])]
       (is (= 3 (count results)))
       (is (= :cqp (:type error)))
       (is (str/includes? (:message error) "bogus"))
       (is (= 0 exit))))))

(deftest stderr->outcome-test
  (testing "registry diagnostics are warnings, the rest is the error"
    (is (= {:warnings ["CL warning: ID field 'x' does not match name of y"
                       "REGISTRY ERROR (/srv/registry/readme): syntax error"]
            :error    "CQP Error: bad query\n  [pos = <--"}
           (cqp/stderr->outcome
            (str "CL warning: ID field 'x' does not match name of y\n"
                 "CQP Error: bad query\n  [pos = <--\n"
                 "REGISTRY ERROR (/srv/registry/readme): syntax error\n")))))
  (testing "nothing but diagnostics is no error"
    (is (nil? (:error (cqp/stderr->outcome "CL warning: x\n"))))
    (is (= {:warnings [] :error nil} (cqp/stderr->outcome "")))))

(deftest registry-diagnostics-integration-test
  ;; a stray file and an entry whose ID field mismatches its filename make
  ;; every cqp process print diagnostics; the batch must still succeed
  (when-cwb
   (let [source (fs/file (:registry ctx) "probe")
         reg    (temp-registry source {"probe2" (mismatched-entry source)
                                       "readme" "not a registry entry\n"})]
     (let [{:keys [results warnings error]}
           (cqp/run-batch! {:registry reg} ["PROBE;" "size PROBE;"])]
       (is (nil? error))
       (is (= ["1"] (second results)))
       (is (some #(str/starts-with? % "REGISTRY ERROR") warnings))
       (is (some #(str/starts-with? % "CL warning:") warnings))))))

(deftest timeout-integration-test
  (when-cwb
   (is (= :timeout
          (-> (cqp/run-batch! (assoc ctx :timeout-ms 1) ["PROBE;" "\"hund\";"])
              :error :type)))))
