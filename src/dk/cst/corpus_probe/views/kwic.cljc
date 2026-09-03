(ns dk.cst.corpus-probe.views.kwic
  "Hiccup for the KWIC concordance.

  The markup mirrors the structure CQP's own display modes imply (PLAN.md
  §7): the concordance is a table of hits (corpus position, structural
  metadata, right-aligned left context, the match as `<mark>`, right
  context), and every token element carries its full attribute set."
  (:require [clojure.string :as str]))

(defn token-title
  "Tooltip text for token map `m`: its non-word attributes joined by ' · '."
  [m]
  (->> (dissoc m :word :open :close)
       (vals)
       (remove str/blank?)
       (str/join " · ")))

(defn token
  "One token `m` as a span, its annotations in the title attribute."
  [m]
  [:span.token {:title (token-title m)} (:word m)])

(defn tokens
  "The token maps `ms` as spans separated by spaces."
  [ms]
  (interpose " " (map token ms)))

(defn struct-summary
  "A short label for the `structs` map of a hit: the most informative
  annotation value, preferring a title-like attribute."
  [structs]
  (or (some structs [:text_title :text_id])
      (first (vals structs))))

(defn struct-title
  "Tooltip text listing every structural annotation of a hit."
  [structs]
  (->> structs
       (map (fn [[k v]] (str (name k) ": " v)))
       (str/join "\n")))

(defn hit-row
  "One KWIC `hit` as a table row."
  [{:keys [cpos left match right structs] :as hit}]
  [:tr.hit
   [:td.cpos (str cpos)]
   [:th.structs {:scope "row" :title (struct-title structs)}
    (struct-summary structs)]
   [:td.left (tokens left)]
   [:td.match [:mark (tokens match)]]
   [:td.right (tokens right)]])

(defn concordance
  "The KWIC `hits` of one result page as a table."
  [hits]
  [:table.kwic
   [:tbody (map hit-row hits)]])
