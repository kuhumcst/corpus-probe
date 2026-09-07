(ns dk.cst.corpus-probe.test.hiccup
  "Test helpers over hiccup: the two lookup contexts the views translate
  through, and probes into what a view rendered (over
  dk.cst.corpus-probe.hiccup/deep, which the tests refer directly)."
  (:require [dk.cst.corpus-probe.hiccup :refer [deep]]
            [dk.cst.corpus-probe.i18n :as i18n]))

(def en
  "The lookup context of the source language, in which every string is
  its own msgid."
  (i18n/->ui "en"))

(def da
  "The lookup context of the bundled Danish translation."
  (i18n/->ui "da"))

(defn text
  "The strings of hiccup `x`, joined: what it reads as. An attribute
  map is not read."
  [x]
  (apply str (filter string? (tree-seq #(and (coll? %) (not (map? %)))
                                       seq x))))

(defn open-states
  "The open state of every disclosure in hiccup `html`, in order."
  [html]
  (->> (deep html)
       (filter #(and (map? %) (contains? % :open)))
       (map :open)))

(defn hidden-ids
  "The ids of the leaves hidden in hiccup `html`: the value of the
  checkbox of each hidden list item."
  [html]
  (->> (deep html)
       (filter #(and (vector? %) (= :li (first %)) (:hidden (second %))))
       (map #(get-in % [2 1 1 :value]))))

(defn summary-texts
  "The text of every disclosure summary in hiccup `html`, in order, the
  chooser's classed ones included."
  [html]
  (->> (deep html)
       (filter #(and (vector? %)
                     (#{:summary :summary.chooser-summary} (first %))))
       (map #(apply str (filter string? (tree-seq coll? seq (rest %)))))))
