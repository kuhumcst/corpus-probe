(ns dk.cst.corpus-probe.server.export-test
  "The downloads: what an export refuses, how a search that fails
  everywhere answers, and the stream written as the corpora answer."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.server.export :as export]
            [dk.cst.corpus-probe.test.cwb :refer [ctx when-cwb]]
            [taoensso.telemere :as t]))

(deftest export-validation-test
  (let [ctx    {:registry "test/resources"}
        export (fn [file params]
                 (export/serve-export ctx {:path-params  {:file file}
                                           :query-params params}))]
    (testing "an export needs a query and known corpora"
      (is (= 400 (:status (export "kwic.tsv" {:corpus "REGISTRY-PROBE"}))))
      (is (= 400 (:status (export "kwic.tsv" {:q "hund"}))))
      (is (= 400 (:status (export "kwic.tsv" {:corpus "NOPE" :q "hund"})))))
    (testing "a frequency export needs corpora"
      (is (= 400 (:status (export "frequencies.csv" {})))))
    (testing "a file that is not a view of a result in a format is not found"
      (is (= 404 (:status (export "kwic.xls" {:corpus "REGISTRY-PROBE"
                                               :q "hund"}))))
      (is (= 404 (:status (export "nonesuch.tsv" {:corpus "REGISTRY-PROBE"
                                                   :q "hund"}))))
      (is (= 404 (:status (export nil {:corpus "REGISTRY-PROBE" :q "hund"})))))))

(deftest export-failure-test
  (testing "a search that fails everywhere is a 400 with the reasons"
    (let [{:keys [status headers body]}
          (t/with-min-level :fatal
            (export/serve-export {:registry "test/resources" :cqp "no-such-cqp"}
                                 {:path-params  {:file "kwic.tsv"}
                                  :query-params {:corpus "REGISTRY-PROBE"
                                                 :q      "hund"}}))]
      (is (= 400 status))
      (is (str/starts-with? (get headers "Content-Type") "text/plain"))
      (is (str/starts-with? body "REGISTRY-PROBE: internal")))))

(deftest export-stream-test
  (when-cwb
   (let [download (fn [file params]
                    (let [{:keys [status headers body]}
                          (export/serve-export ctx {:path-params  {:file file}
                                                    :query-params params})
                          out (java.io.ByteArrayOutputStream.)]
                      (body out)
                      {:status  status
                       :type    (get headers "Content-Type")
                       :text    (String. (.toByteArray out) "UTF-8")}))
         {:keys [status type text]} (download "kwic.tsv" {:corpus "PROBE,TALER"
                                                          :q      "hund"})
         lines (str/split-lines text)]
     (testing "the download is written as the corpora answer, under the
               columns of them all"
       (is (= 200 status))
       (is (str/starts-with? type "text/tab-separated-values"))
       (is (= (str "corpus\tcpos\tmatchend\tleft\tmatch\tright\tmatch pos\t"
                   "match lemma\ts_id\ttext_id\ttext_title\ttext_year\t"
                   "text_speaker\ttext_party")
              (first lines)))
       (is (some #{(str "PROBE\t9\t9\t. Katten jagter en lille\thund\t"
                        "i haven . Hunde og\tNCSI\thund\t2\tt1\tHverdag\t"
                        "2023\t\t")}
                 lines)))
     (testing "as CSV, with the byte order mark and CRLF"
       (let [{:keys [text]} (download "kwic.csv" {:corpus "PROBE" :q "hund"})]
         (is (str/starts-with? text "﻿corpus,cpos,matchend,"))
         (is (str/includes? text "\r\n")))))))
