(ns dk.cst.corpus-probe.hiccup
  "Helpers over hiccup that know nothing of this app: walking a form,
  marking the element a fragment names, and the headings."
  (:require [clojure.walk :as walk]))

(defn deep
  "All nodes of hiccup `form`, descending into attribute maps too, so a
  caller can look for attribute values as well as tags and text."
  [form]
  (tree-seq coll? seq form))

(defn mark-target
  "The hiccup `body` with the content of the element whose id is
  `fragment` in a <mark>: the part a link named, which a deep link near
  the foot of a page cannot scroll to the top. `body` as it is without a
  fragment or a match."
  [fragment body]
  (if fragment
    (walk/postwalk (fn [x]
                     (if (and (vector? x) (map? (second x))
                              (= fragment (:id (second x))))
                       [(first x) (second x) (into [:mark] (drop 2 x))]
                       x))
                   body)
    body))

(def headings
  "The heading tags, outermost first."
  [:h1 :h2 :h3 :h4 :h5 :h6])

(defn heading?
  "True when hiccup `x` is a heading."
  [x]
  (and (vector? x) (boolean (some #{(first x)} headings))))

(defn heading-text
  "The text of heading `h` (see `heading?`): its strings joined, however
  nested, its attributes left out."
  [[_ attrs & children :as h]]
  (let [content (if (map? attrs) children (rest h))]
    (apply str (filter string? (tree-seq coll? seq content)))))

(defn heading
  "The heading tag `level` deep, h6 at the deepest: HTML has no h7."
  [level]
  (keyword (str "h" (min 6 level))))
