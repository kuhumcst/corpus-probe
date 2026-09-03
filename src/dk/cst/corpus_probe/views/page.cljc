(ns dk.cst.corpus-probe.views.page
  "Hiccup for the search page: layout, search form, result summary and
  pagination. Shared .cljc so the coming client can render the same views."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.views.kwic :as kwic]))

(defn search-form
  "The search form over `corpora` (corpus maps), prefilled from `params`
  (:corpus :q :mode :ci :prefix :suffix).

  Submits as GET so every search has a shareable URL. The mode selects
  between raw CQP and the simple-search compiler; the checkboxes apply to
  simple mode only."
  [corpora {:keys [corpus q mode ci prefix suffix] :as params}]
  [:form.search {:method "get" :action "/"}
   [:label {:for "corpus"} "Corpus"]
   [:select {:id "corpus" :name "corpus"}
    (for [{:keys [id]} corpora
          :let [value (str/upper-case id)]]
      [:option {:value    value
                :selected (= value corpus)}
       value])]
   [:label {:for "q"} "Query"]
   [:input {:id           "q"
            :name         "q"
            :type         "text"
            :value        (or q "")
            :placeholder  "[lemma = \"hund\"] or plain words"
            :autocomplete "off"
            :spellcheck   "false"}]
   [:fieldset.mode
    [:label [:input {:type    "radio" :name "mode" :value "cqp"
                     :checked (not= mode "simple")}]
     "CQP"]
    [:label [:input {:type    "radio" :name "mode" :value "simple"
                     :checked (= mode "simple")}]
     "Simple"]
    [:label [:input {:type    "checkbox" :name "ci" :value "on"
                     :checked (some? ci)}]
     "ignore case"]
    [:label [:input {:type    "checkbox" :name "prefix" :value "on"
                     :checked (some? prefix)}]
     "starts with"]
    [:label [:input {:type    "checkbox" :name "suffix" :value "on"
                     :checked (some? suffix)}]
     "ends with"]]
   [:button {:type "submit"} "Search"]])

(defn result-summary
  "One line summarising a KWIC `result` page."
  [{:keys [size corpus page page-size] :as result}]
  (let [pages (max 1 (int (Math/ceil (/ size (double page-size)))))]
    [:p.summary
     (str size " " (if (= 1 size) "hit" "hits") " in " corpus
          " · page " (inc page) " of " pages)]))

(defn pagination
  "Prev/next links for a `result` page; the hrefs are computed server-side
  and passed in as `prev-href` and `next-href` (nil when out of range)."
  [result prev-href next-href]
  (when (or prev-href next-href)
    [:nav.pagination
     (if prev-href [:a {:href prev-href} "← previous"] [:span])
     (if next-href [:a {:href next-href} "next →"] [:span])]))

(defn error-section
  "A CQP `error` map rendered verbatim. cqp's own messages, including the
  `<--` position pointer, are shown to the user unchanged."
  [{:keys [type message] :as error}]
  [:section.error
   [:h2 (case type
          :timeout "The query timed out"
          "CQP error")]
   (when message [:pre message])])

(defn page
  "The whole search page: form from `corpora`/`params`, then either the
  `error`, the KWIC `result` (with `prev-href`/`next-href` pagination) or
  nothing when no query was made."
  [{:keys [corpora params result error prev-href next-href]}]
  [:main
   [:header
    [:h1 "corpus-probe"]
    [:p.subtitle "CWB corpus search"]]
   (search-form corpora params)
   (cond
     error  (error-section error)
     result [:section.result
             (result-summary result)
             (kwic/concordance (:hits result))
             (pagination result prev-href next-href)]
     :else  nil)])
