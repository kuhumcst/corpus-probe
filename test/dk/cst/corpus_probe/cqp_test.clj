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
  "True when the cqp binary and the encoded dev corpus are both present;
  the integration tests below are skipped otherwise."
  (boolean (and (fs/which "cqp")
                (.exists (io/file (:registry ctx) "probe")))))

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

(deftest timeout-integration-test
  (when-cwb
   (is (= :timeout
          (-> (cqp/run-batch! (assoc ctx :timeout-ms 1) ["PROBE;" "\"hund\";"])
              :error :type)))))
