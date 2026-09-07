(ns dk.cst.corpus-probe.hiccup-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.hiccup :as hiccup]))

(deftest deep-test
  (let [nodes (hiccup/deep [:p {:id "a"} [:em "x"]])]
    (testing "tags, text, attribute maps and their values alike are found"
      (is (some #{:em} nodes))
      (is (some #{"x"} nodes))
      (is (some #{{:id "a"}} nodes))
      (is (some #{"a"} nodes)))))

(deftest mark-target-test
  (testing "the whole content of the element goes in the mark"
    (is (= [[:dt {:id "a"} [:mark "x " [:code "y"]]]]
           (hiccup/mark-target "a" [[:dt {:id "a"} "x " [:code "y"]]]))))
  (testing "nothing is marked without a fragment or with one nothing carries"
    (let [body [[:dt {:id "a"} "x"]]]
      (is (= body (hiccup/mark-target nil body)))
      (is (= body (hiccup/mark-target "b" body))))))

(deftest heading-test
  (is (hiccup/heading? [:h2 {:id "a"} "x"]))
  (is (hiccup/heading? [:h1 "x"]))
  (is (not (hiccup/heading? [:p "x"])))
  (is (not (hiccup/heading? "x")))
  (testing "the text of a heading, however nested, its attributes left out"
    (is (= "The cpos column"
           (hiccup/heading-text
            [:h2 {:id "a"} "The " [:code "cpos"] " column"])))
    (is (= "x" (hiccup/heading-text [:h1 "x"]))))
  (testing "a level too deep for HTML's headings keeps the last"
    (is (= :h6 (hiccup/heading 7)))
    (is (= :h2 (hiccup/heading 2)))))
