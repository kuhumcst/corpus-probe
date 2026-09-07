(ns dk.cst.corpus-probe.server.corpora-test
  "The corpus pages over the registry fixture: hostile names refused,
  entries found case-insensitively, and paths kept private."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.server.corpora :as corpora]))

(deftest serve-corpus-test
  (let [ctx  {:registry "test/resources"}
        page (fn [id] (corpora/serve-corpus ctx {:path-params {:id id}
                                                 :headers {"cookie" "lang=en"}}))]
    (testing "a hostile corpus name is rejected before touching anything"
      (is (= 404 (:status (page "bad; exit"))))
      (is (= 404 (:status (page "../resources/registry-probe")))))
    (testing "a name with no registry entry is not found"
      (is (= 404 (:status (page "nope")))))
    (testing "a directory is not a corpus"
      (is (= 404 (:status (page "golden")))))
    (testing "an entry is found case-insensitively and its paths stay private"
      ;; the fixture's data files do not exist, so the page is the alert
      (let [{:keys [status body]} (page "Registry-Probe")]
        (is (= 200 status))
        (is (str/includes? body "Unreadable corpus"))
        (is (not (str/includes? body "/corpora/data/probe")))))))

(deftest valueless-param-test
  (testing "a query param written without a value does not fail the index"
    (let [ctx {:registry "test/resources"}]
      (is (= 200 (:status (corpora/serve-corpora ctx {:uri "/corpora"
                                                      :query-params
                                                      {nil "foo"}})))))))

(deftest serve-text-test
  (let [page (fn [id params]
               (corpora/serve-text {:registry "test/resources"}
                                   {:path-params  {:id id}
                                    :query-params params
                                    :headers      {"cookie" "lang=en"}}))]
    (testing "a hostile or unknown corpus, or no position, is not found"
      (is (= 404 (:status (page "bad; exit" {:cpos "9"}))))
      (is (= 404 (:status (page "nope" {:cpos "9"}))))
      (is (= 404 (:status (page "registry-probe" {:cpos "nine"})))))
    (testing "without a position the reader is sent to the corpus page"
      (is (= 303 (:status (page "registry-probe" {}))))
      (is (= "/corpora/registry-probe"
             (get-in (page "registry-probe" {:cpos ""}) [:headers "Location"]))))
    (testing "a corpus whose data are gone is a page saying so, and its
              paths stay private"
      (let [{:keys [status body]} (page "registry-probe" {:cpos "9"})]
        (is (= 200 status))
        (is (str/includes? body "CQP error"))
        (is (not (str/includes? body "/corpora/data/probe")))))))
