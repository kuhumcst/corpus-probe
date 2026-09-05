(ns dk.cst.corpus-probe.views.text
  "Hiccup for the reading page: one text of a corpus as running prose,
  its metadata before it and the hit the reader came from marked, so
  that a concordance line can be read in full.

  The page is a document like the frontpage, so the stylesheet gives it
  a measure; its blocks are the paragraphs the corpus marks, or its
  sentences where it marks none, each a <p>. The words are text and
  nothing more: reading is the task here, and the concordance is where
  a token is inspected."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]))

(defn text-name
  "What the text with structural annotations `structs` is called, in
  `ui`: its title, else its id, else just a text."
  [ui structs]
  (or (:text_title structs) (:text_id structs) (i18n/tr ui "Text")))

(defn marked
  "The `words` of one block of a text starting at corpus position
  `from`, as text, those from `cpos` to `matchend` (the `hit`; nil for
  none) inside a <mark>, which carries `url/hit-id` when `landing?`, so
  that the page's URL lands on it."
  [from [cpos matchend :as hit] landing? words]
  (if hit
    (->> (map-indexed (fn [i word] [(<= cpos (+ from i) matchend) word]) words)
         (partition-by first)
         (map (fn [run]
                (let [text (str/join " " (map second run))]
                  (if (ffirst run)
                    [:mark (cond-> {} landing? (assoc :id url/hit-id)) text]
                    text))))
         (interpose " "))
    (str/join " " words)))

(defn reading-view
  "The reading page's main content from `data` (see
  dk.cst.corpus-probe.search/text!, plus the `:hit` [cpos matchend] to
  mark and the `:lang` of the corpus text), in `ui`: the text's name,
  the corpus it is from, its structural annotations and its `:blocks`
  as paragraphs, the hit marked in the block that holds it (see
  `marked`); or the `:error` that came instead of the text."
  [ui {:keys [corpus structs blocks from hit lang error] :as data}]
  [:main.document layout/main-attrs
   [:h1 (text-name ui structs)]
   (if error
     [:section.error
      [:h2 (page/error-name ui error)]
      (page/error-body ui error [corpus])]
     (list
      [:p (i18n/tr ui "in") " " [:a {:href (url/corpus corpus)} [:code corpus]]]
      (page/attribute-list structs)
      ;; the corpus text is in its own language while the page around it
      ;; is in the reader's
      [:div (cond-> {} lang (assoc :lang lang))
       (map (fn [block-from words]
              (let [block-to (+ block-from (dec (count words)))]
                [:p (marked block-from hit
                            (and hit (<= block-from (first hit) block-to))
                            words)]))
            (reductions + from (map count blocks))
            blocks)]))])
