(ns dk.cst.corpus-probe.stats-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.stats :as stats]))

(deftest per-million-test
  (is (= 106383.0 (stats/per-million 5 47)))
  (is (= 0.5 (stats/per-million 1 2000000)))
  (testing "an empty corpus has no rate"
    (is (nil? (stats/per-million 0 0)))))
