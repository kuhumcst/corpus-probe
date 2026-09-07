(ns dk.cst.corpus-probe.cwb-test
  "Protocol tests against the captured child-mode session, plus live
  integration tests that run when cqp and the dev corpus are available."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.test.cwb
             :refer [ctx da-collator mismatched-entry temp-registry! when-cwb]]
            [taoensso.telemere :as t]))

(deftest commands->stdin-test
  (is (= "PROBE;\n.EOL.;\nsize Last;\n.EOL.;\n"
         (cwb/commands->stdin ["PROBE;" "size Last;"]))))

(deftest stdout->sections-test
  (testing "captured session splits into banner + aligned sections"
    (let [{:keys [banner sections]}
          (cwb/stdout->sections (slurp "test/resources/golden/session-raw.stdout"))]
      (is (= "CQP version 3.5.0" banner))
      (is (= 5 (count sections)))
      (testing "silent commands yield empty sections, incl. the failed one"
        (is (= [[] [] ["5"] []] (vec (butlast sections)))))
      (is (= 2 (count (last sections))))))
  (testing "output without a banner"
    (is (= {:banner nil :sections [["x"]]}
           (cwb/stdout->sections "x\n-::-EOL-::-\n"))))
  (testing "progress lines are filtered"
    (is (= [["5"]]
           (:sections (cwb/stdout->sections
                       "-::-PROGRESS-::-\t1\t1\t 50% complete\n5\n-::-EOL-::-\n"))))))

(deftest version-integration-test
  (when-cwb
   (is (str/starts-with? (cwb/version! ctx) "CQP version"))))

(deftest run!-test
  (is (= {:out "hi\n" :err "" :exit 0} (cwb/run! ["echo" "hi"] 5000 {})))
  (testing "a process that outlives its budget is a timeout, not a result"
    (is (cwb/timeout? (cwb/run! ["sleep" "5"] 10 {})))))

(deftest timeout?-test
  (testing "a run, a batch and an exception thrown for one all say it"
    (is (cwb/timeout? {:timeout? true}))
    (is (cwb/timeout? {:error {:type :timeout}}))
    (is (cwb/timeout? (ex-info "x" {:error {:type :timeout}}))))
  (testing "nothing else does"
    (is (not (cwb/timeout? {:out "" :err "" :exit 0})))
    (is (not (cwb/timeout? {:error {:type :cqp}})))
    (is (not (cwb/timeout? (ex-info "x" {}))))))

(deftest run-batch-integration-test
  (when-cwb
   (testing "sections align with commands even when one fails"
     (let [{:keys [results error exit]}
           (cwb/run-batch! ctx ["PROBE;" "bogus;" "size Last;"])]
       (is (= 3 (count results)))
       (is (= :cqp (:type error)))
       (is (str/includes? (:message error) "bogus"))
       (is (= 0 exit))))))

(deftest batch-integration-test
  (when-cwb
   (testing "the sections of a batch that runs"
     (is (= [[] ["1"]]
            (cwb/batch! ctx "PROBE" "size PROBE;" ["PROBE;" "size PROBE;"]))))
   (testing "and the error of one that fails, naming what ran"
     (let [e (try (cwb/batch! ctx "PROBE" "bogus;" ["PROBE;" "bogus;"])
                  (catch Exception e (ex-data e)))]
       (is (= :cqp (-> e :error :type)))
       (is (= "PROBE" (:corpus e)))
       (is (= "bogus;" (:query e)))))))

(deftest stderr->outcome-test
  (testing "registry diagnostics are warnings, the rest is the error"
    (is (= {:warnings ["CL warning: ID field 'x' does not match name of y"
                       "REGISTRY ERROR (/srv/registry/readme): syntax error"]
            :error    "CQP Error: bad query\n  [pos = <--"}
           (cwb/stderr->outcome
            (str "CL warning: ID field 'x' does not match name of y\n"
                 "CQP Error: bad query\n  [pos = <--\n"
                 "REGISTRY ERROR (/srv/registry/readme): syntax error\n")))))
  (testing "nothing but diagnostics is no error"
    (is (nil? (:error (cwb/stderr->outcome "CL warning: x\n"))))
    (is (= {:warnings [] :error nil} (cwb/stderr->outcome "")))))

(deftest registry-diagnostics-integration-test
  ;; a stray file and an entry whose ID field mismatches its filename make
  ;; every cqp process print diagnostics; the batch must still succeed
  (when-cwb
   (let [source (fs/file (:registry ctx) "probe")
         reg    (temp-registry! source {"probe2" (mismatched-entry source)
                                        "readme" "not a registry entry\n"})]
     (let [{:keys [results warnings error]}
           (cwb/run-batch! {:registry reg} ["PROBE;" "size PROBE;"])]
       (is (nil? error))
       (is (= ["1"] (second results)))
       (is (some #(str/starts-with? % "REGISTRY ERROR") warnings))
       (is (some #(str/starts-with? % "CL warning:") warnings))))))

(deftest timeout-integration-test
  (when-cwb
   (is (= :timeout
          (-> (cwb/run-batch! (assoc ctx :timeout-ms 1) ["PROBE;" "\"hund\";"])
              :error :type)))))

(deftest public-error-test
  (testing "CL warning lines (which may name server paths) are dropped"
    (is (= {:type :cqp :message "CQP Error: bad query\n  [pos = <--"}
           (cwb/public-error
            {:type    :cqp
             :message (str "CL warning: ID field 'x' does not match name of "
                           "registry file /srv/registry/y\n"
                           "CQP Error: bad query\n  [pos = <--")}))))
  (testing "a message of nothing but warnings empties out"
    (is (nil? (:message (cwb/public-error {:message "CL warning: x"})))))
  (testing "an error without a message passes through"
    (is (= {:type :timeout} (cwb/public-error {:type :timeout}))))
  (testing "follow-on errors after the failing command's own are dropped"
    (is (= "CQP Error:\n\tCorpus ``NOSUCH'' is undefined"
           (:message (cwb/public-error
                      {:message (str "CQP Error:\n\tCorpus ``NOSUCH'' is "
                                     "undefined\n"
                                     (str/join "\n" cwb/follow-on-errors))})))))
  (testing "a failed filter leaves only its own error"
    (is (= "CQP Error:\n\tStructural attribute X.text_author does not exist."
           (:message (cwb/public-error
                      {:message (str "CQP Error:\n\tStructural attribute "
                                     "X.text_author does not exist.\n"
                                     "CQP Error:\n\tCorpus ``Last'' is "
                                     "undefined\n"
                                     "CQP Error:\n\tCorpus ``Filter'' is "
                                     "undefined")})))))
  (testing "a message of nothing but follow-on errors is kept"
    (let [message (first cwb/follow-on-errors)]
      (is (= message (:message (cwb/public-error {:message message})))))))

(deftest error-map-test
  (testing "a CQP error travels as it is"
    (is (= {:type :cqp :message "x"}
           (cwb/error-map (ex-info "failed" {:error {:type :cqp
                                                     :message "x"}})))))
  (testing "one of our own guards is a rejection with its message"
    (is (= {:type :rejected :message "Invalid corpus name"}
           (cwb/error-map (ex-info "Invalid corpus name" {})))))
  (testing "any other exception is internal, its message withheld"
    (is (= {:type :internal}
           (t/with-min-level :fatal
             (cwb/error-map (java.io.IOException. "/srv/secret")))))))

(deftest attempt-test
  (is (= {:corpus "X" :size 1}
         (cwb/attempt "X" (fn [] {:corpus "X" :size 1}))))
  (testing "a failure is the corpus's error map"
    (is (= {:corpus "X" :error {:type :rejected :message "no"}}
           (cwb/attempt "X" #(throw (ex-info "no" {}))))))
  (testing "and what the caller reads from the exception beside it"
    (is (= {:corpus "X" :error {:type :cqp :message "m"} :phantom? true}
           (cwb/attempt "X"
                        #(throw (ex-info "no" {:error    {:type    :cqp
                                                          :message "m"}
                                               :phantom? true}))
                        (fn [e] {:phantom? (:phantom? (ex-data e))}))))))

(deftest pmap-n-test
  (is (= [1 2 3 4 5] (cwb/pmap-n 2 inc (range 5))))
  (testing "at most n calls run at once"
    (let [running (atom 0) peak (atom 0)]
      (dorun (cwb/pmap-n 3 (fn [_]
                             (swap! peak max (swap! running inc))
                             (Thread/sleep 20)
                             (swap! running dec))
                         (range 40)))
      (is (<= @peak 3)))))

(deftest locale-test
  (testing "an LC_ALL value names its language and territory"
    (is (= "da" (.getLanguage (cwb/locale "da_DK.UTF-8"))))
    (is (= "DK" (.getCountry (cwb/locale "da_DK.UTF-8"))))
    (is (= "en" (.getLanguage (cwb/locale "en_US")))))
  (testing "a value naming no locale is the root one"
    (is (= java.util.Locale/ROOT (cwb/locale "C")))
    (is (= java.util.Locale/ROOT (cwb/locale "")))
    (is (= java.util.Locale/ROOT (cwb/locale nil)))))

(deftest collator-test
  (testing "Danish sorts æ, ø and å after z, not among the vowels"
    (is (= ["and" "brød" "zoo" "ægte" "øl" "århus"]
           (sort @da-collator
                 ["øl" "ægte" "zoo" "århus" "and" "brød"]))))
  (testing "an installation with no sort locale still sorts"
    (is (= ["a" "z"] (sort (cwb/->collator {}) ["z" "a"])))))

(deftest running-ctx-test
  (testing "a batch that runs the query gets the longer timeout"
    (is (= 900000
           (:timeout-ms (cwb/running-ctx {:timeout-ms       60000
                                          :query-timeout-ms 900000})))))
  (testing "with none configured, every batch keeps the ordinary timeout"
    (is (= 60000 (:timeout-ms (cwb/running-ctx {:timeout-ms 60000}))))
    (is (nil? (:timeout-ms (cwb/running-ctx {}))))))

(deftest within-deadline-test
  (let [soon (+ (System/currentTimeMillis) 5000)
        ctx  (cwb/within-deadline {:timeout-ms       60000
                                   :query-timeout-ms 300000}
                                  soon)]
    (testing "no batch may outlive the budget the search was given"
      (is (<= (:timeout-ms ctx) 5000))
      (is (<= (:query-timeout-ms ctx) 5000))))
  (testing "a deadline further off than the timeouts leaves them alone"
    (let [far (+ (System/currentTimeMillis) 600000)
          ctx (cwb/within-deadline {:timeout-ms       60000
                                    :query-timeout-ms 300000} far)]
      (is (= 60000 (:timeout-ms ctx)))
      (is (= 300000 (:query-timeout-ms ctx)))))
  (testing "a deadline already past still leaves a floor to fail in"
    (let [ctx (cwb/within-deadline {:timeout-ms 60000}
                                   (- (System/currentTimeMillis) 10000))]
      (is (= 1000 (:timeout-ms ctx)))))
  (testing "timeouts that were never configured are not invented"
    (let [soon (+ (System/currentTimeMillis) 5000)]
      (is (= {} (cwb/within-deadline {} soon))))))
