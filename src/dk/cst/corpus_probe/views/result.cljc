(ns dk.cst.corpus-probe.views.result
  "What both views of a search result share: the results region with its
  heading and the phrases naming what was asked, the switch between the
  views, the controls over a result, the pager, the download links and
  the error sections. Nothing here knows the search form: the region
  answers the params a search was asked with, which the form may have
  moved on from."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.tokens :as tokens]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(defn query-phrase
  "The query of `params` in words for a title, in `ui`: the text as
  typed; for a list, how many words it holds, a title being one line;
  for an extended search, the CQP its tokens compile to."
  [ui {:keys [q] :as params}]
  (case (mode/mode params)
    "list"     (let [n (count (query/words q))]
                 (str n " " (i18n/trn ui "word" "words" n)))
    "extended" (query/->cqp (query/of params))
    q))

(defn form-query
  "The query the search form holds: that of its `params`, or in the
  extended mode of its `tokens` as the client keeps them, within the
  unit the params name."
  [params tokens]
  (let [mode (mode/mode params)]
    (query/of (if (= "extended" mode)
                (assoc (tokens/rows->params tokens)
                       :mode mode :within (:within params))
                params))))

(defn match-label
  "What the `match` param value is called, in `ui`: how much of the
  form a simple search must cover; the whole word for a value naming
  none."
  [ui match]
  (case match
    "prefix" (i18n/tr ui "start of word")
    "suffix" (i18n/tr ui "end of word")
    "infix"  (i18n/tr ui "part of word")
    (i18n/tr ui "whole word")))

(defn asked?
  "True when the search `params` ask for anything; a search asking
  nothing counts every token, which only a frequency table wants."
  [params]
  (some? (query/of params)))

(defn counting?
  "True while corpora of `result` are `:remaining` to be counted, its
  size being the hits counted so far."
  [{:keys [remaining] :as result}]
  (boolean (seq remaining)))

(defn searched?
  "True when any corpus of `result` could be searched or is still being
  counted, so its counts are an answer rather than a report of failure."
  [{:keys [counts] :as result}]
  (boolean (or (some :size counts) (counting? result))))

(defn hits-phrase
  "The number of hits `n` in words in `ui`."
  [ui n]
  (str (i18n/group-digits ui n) " "
       (i18n/trn ui "hit" "hits" n)))

(defn hits-heading
  "What a search found, as the heading of its result in `ui`: how many
  hits, `size`, at least that many while `counting?`; every token when
  nothing was `asked?` of `params`."
  ([ui params size]
   (hits-heading ui params size false))
  ([ui params size counting?]
   (if (asked? params)
     (str (when counting? (str (i18n/tr ui "at least") " "))
          (hits-phrase ui size))
     (i18n/tr ui "All tokens"))))

(defn query-mark
  "The query of `params` as the answer names it, in `ui`: CQP, and the
  CQP an extended search compiles to, as code, a list as how many words
  it holds, and a simple search quoted, a word spoken of, not used."
  [ui params]
  (let [phrase (query-phrase ui params)]
    (case (mode/mode params)
      ("cqp" "extended") [:code phrase]
      "list"             phrase
      [:q phrase])))

(defn question
  "The query the result of `state` answered, in `ui`, once the form
  above has moved on from it: its `:asked` params are the ones the
  search ran with, the form's query is read from its `:params` and
  `:tokens`. Nil while the form still holds the query."
  [ui {:keys [asked params tokens]}]
  (when (not= (query/of asked) (form-query params tokens))
    (query-mark ui asked)))

(defn corpora-phrase
  "The names of `corpora` in words in `ui`: the one name, or how many
  there were."
  [ui corpora]
  (if (= 1 (count corpora))
    (first corpora)
    (str (count corpora) " " (i18n/tr ui "corpora"))))

(defn page-phrase
  "Where in a paged `result` the reader is, in `ui`: the page, and of how
  many once the result is counted (see `counting?`)."
  [ui {:keys [page pages] :as result}]
  (str (i18n/tr ui "page") " " (inc page)
       (when pages (str " " (i18n/tr ui "of") " " pages))))

(defn sample-phrase
  "That a result holds a random `sample` of the matches rather than all
  of them, in `ui`, over `corpora`; nil when it holds them all. The
  number is the sample asked for, one drawn per corpus, not the hits
  that came back."
  [ui sample corpora]
  (when sample
    (str (i18n/tr ui "a random sample of at most") " "
         (i18n/group-digits ui sample)
         (when (next corpora) (str " " (i18n/tr ui "per corpus"))))))

(defn position-label
  "What the `position` of a match is called, in `ui`, worded to follow
  an attribute name; the position itself for one nothing names."
  [ui position]
  (case position
    "match[-1]"       (i18n/tr ui "before the match")
    "match"           (i18n/tr ui "at the start of the match")
    "match..matchend" (i18n/tr ui "over the whole match")
    "matchend"        (i18n/tr ui "at the end of the match")
    "matchend[1]"     (i18n/tr ui "after the match")
    position))

(defn subset-phrase
  "That a result holds only the hits whose token at the :anchor of
  `subset` has its :value as its :attr, in `ui`; nil without one."
  [ui {:keys [anchor attr value] :as subset}]
  (when subset
    (list [:code (name attr)] " " (position-label ui anchor) " = "
          [:code value])))

(defn subset-inputs
  "The `subset` narrowing as hidden inputs, so the form carries it; nil
  without one.

  It reaches a result by link alone (see
  dk.cst.corpus-probe.url/subset-href) and has no control of its own, so
  without these a change of sort or context drops it without a word."
  [{:keys [anchor attr value] :as subset}]
  (when subset
    (list [:input {:type "hidden" :name "subset" :value value}]
          [:input {:type "hidden" :name "subset-at" :value anchor}]
          [:input {:type "hidden" :name "subset-attr" :value (name attr)}])))

(defn near-phrase
  "That a result holds only the hits with the :word of `near` nearby,
  in `ui`; nil without one."
  [ui {:keys [word] :as near}]
  (when near
    (list (i18n/tr ui "near") " " [:code word])))

(defn filter-phrase
  "How a search was narrowed by metadata, in words: each attribute of
  `narrowing` with the values `:filter` accepts, sorted, then the
  regexes `:patterns` accepts between slashes and the [from to] of
  `:ranges`; empty when nothing narrows it.

  (filter-phrase {:filter {:text_year #{\"1591\"}}
                  :patterns {:text_title [\"Hav.*\"]}})
  ;; => \"text_title /Hav.*/; text_year 1591\""
  [{:keys [filter patterns ranges] :as narrowing}]
  (str/join "; " (for [attr (sort (distinct (concat (keys filter)
                                                    (keys patterns)
                                                    (keys ranges))))
                       :let [[from to] (get ranges attr)]]
                   (str (name attr) " "
                        (str/join ", "
                                  (concat (sort (get filter attr))
                                          (map #(str "/" % "/")
                                               (get patterns attr))
                                          (when from
                                            [(str from "–" to)])))))))

(defn within-phrase
  "The `narrowing` of a result (see `filter-phrase`) as a qualifier of it
  in `ui`, or nil without one: \"within text_year 1591\"."
  [ui {:keys [filter patterns ranges] :as narrowing}]
  (when (or (seq filter) (seq patterns) (seq ranges))
    (str (i18n/tr ui "within") " " (filter-phrase narrowing))))

;; TODO: should any of these phrases be optional, kept as a stored setting?
(defn qualifiers
  "The question a `result` answered, less the query itself, as short
  phrases in `ui`: the attribute a simple search of `params` matched and
  the part of the form, when not the usual ones; the corpora searched;
  the metadata filter; the narrowings; the sample."
  [ui {:keys [in match] :as params}
   {:keys [counts size sample remaining] :as result}]
  (let [searched (concat (map :corpus (filter :size counts)) remaining)
        ;; an option the mode does not read may still ride in the params,
        ;; as memory for the form's disabled control; the search never saw
        ;; it, so the line must not say it did
        reads?   (partial mode/reads? (mode/mode params))]
    (remove nil?
            [(when (and (reads? :in) (not (contains? #{nil "" "word"} in)))
               (list (i18n/tr ui "attribute") " " [:code in]))
             (when (and (reads? :match) (not (str/blank? match)))
               (match-label ui match))
             (when (seq searched)
               (str (i18n/tr ui "in") " " (corpora-phrase ui searched)))
             (within-phrase ui result)
             (subset-phrase ui (:subset result))
             (near-phrase ui (:near result))
             ;; a search that found nothing sampled nothing, and saying it
             ;; drew a sample of what it found reads as the reason it is
             ;; empty
             (when (pos? (or size 0))
               (sample-phrase ui sample searched))])))

(defn view-label
  "What the result view `k` is called, in `ui`: the concordance is KWIC,
  the word bare rather than in an <abbr>. A title inside a link leaves
  the link no name of its own, and a tooltip is a thing no keyboard
  reaches; the glossary is where the jargon is spelled out."
  [ui k]
  (case k
    :kwic        "KWIC"
    :frequencies (i18n/tr ui "Frequencies")
    (name k)))

(defn view-switch
  "The switch between the views of one result in `ui`: each of `hrefs`
  ([view url]) as a link, `view` marked as the one being shown; nil
  without hrefs."
  [ui view hrefs]
  (when (seq hrefs)
    ;; links, not a tablist: each view is its own URL and its own question
    ;; to CQP, so following one is a navigation, not a panel already loaded
    (widgets/tabs (i18n/tr ui "Result view")
                  (for [[k href] hrefs] [k href (view-label ui k)])
                  view)))

(defn apply-button
  "The button applying a result's controls where no `client?` runs to
  apply them itself, in `ui`; nil where one does."
  [ui client?]
  (when-not client?
    ;; inside noscript: rendered bare, a scripted browser flashes it for
    ;; the split second before the client's first render takes it away
    (list " " [:noscript
               [:button {:type "submit" :form url/form-id}
                (i18n/tr ui "Apply")]])))

(defn view-controls
  "The controls of a result in `ui`: the `reading` ones (hiccup), which
  decide how the hits are read, and behind a disclosure the `narrowing`
  ones (hiccup; nil for none), which keep only some of them, `open?`
  saying whether that disclosure starts open, as it must while a
  narrowing is in force; each row with the `apply-button` where no
  `client?` runs. Nil without either."
  [ui client? reading narrowing open?]
  (when (or reading narrowing)
    [:div.view-controls
     (when reading
       [:p reading (apply-button ui client?)])
     (when narrowing
       [:details {:open (boolean open?)}
        [:summary (i18n/tr ui "Narrow the result")]
        [:p narrowing (apply-button ui client?)]])]))

(def near-distances
  "The distances the near control offers, in display order."
  [1 2 3 5 10])

(defn near-control
  "The proximity control of a result in `ui`: the word every hit must
  have nearby and how many words away it may be, from the :word and
  :distance of `near`, over the `near-distances`."
  [ui {:keys [word distance]}]
  (let [distance (or distance url/default-distance)
        words    (fn [n] (str n " " (i18n/trn ui "word" "words" n)))]
    (list
     [:label {:for "near"} (i18n/tr ui "Near")]
     " "
     ;; applies on change, not on Enter alone: implicit submission does
     ;; not reach a form from a field that only names it
     [:input {:id           "near"
              :name         "near"
              :type         "search"
              :form         url/form-id
              :value        (or word "")
              :autocomplete "off"
              :on           {:change [:apply-view]}}]
     " "
     (widgets/select url/form-id "distance" (i18n/tr ui "within")
                     (for [n (sort (conj (set near-distances) distance))]
                       (widgets/option distance n (words n)))))))

(defn pager-links
  "The page links of a result around `position` (where in the sequence
  the reader is), labelled in `ui`; nil when neither `prev-href` nor
  `next-href` is in range."
  [ui prev-href next-href position]
  (widgets/pager (when prev-href
                   [prev-href (str "← " (i18n/tr ui "previous"))])
                 (when next-href
                   [next-href (str (i18n/tr ui "next") " →")])
                 position))

(defn pagination
  "The `pager-links` around `position` from `prev-href` and `next-href`,
  as a navigation landmark named in `ui`."
  [ui prev-href next-href position]
  (when-let [links (pager-links ui prev-href next-href position)]
    ;; the one pager that is a landmark: two landmarks named Pagination
    ;; cannot be told apart, so the repeat below the table is the bare list
    [:nav.pagination.menu {:aria-label (i18n/tr ui "Pagination")} links]))

(defn download-links
  "Links downloading the current table in each format of `hrefs` (format
  keyword to URL), with `note` (when given) qualifying what the download
  holds; nil without hrefs, worded in `ui`."
  [ui hrefs note]
  (when (seq hrefs)
    [:p.downloads (i18n/tr ui "Download")
     (when note (str " " note))
     ": "
     (interpose " · "
                (for [[format href] (sort hrefs)]
                  ;; no download attribute: the response's own
                  ;; Content-Disposition asks to be saved
                  [:a {:href href} (str/upper-case (name format))]))]))

(defn error-groups
  "The errors among the per-corpus `counts`, grouped by identical error:
  [[error [corpus ...]] ...] in first-seen order, so a query that fails the
  same way in every corpus is reported once."
  [counts]
  (let [failed (filter :error counts)]
    (for [error (distinct (map :error failed))]
      [error (map :corpus (filter #(= error (:error %)) failed))])))

(defn error-heading
  "What `error` is called, in `ui`, by its type: the types this project
  reports itself, and CQP's own error for anything else, which is headed
  as such and carries its message."
  [ui {:keys [type] :as error}]
  (case type
    :timeout        (i18n/tr ui "The search did not finish in time")
    :no-corpus      (i18n/tr ui "No corpus selected")
    :no-texts       (i18n/tr ui "The corpus marks no texts")
    :unknown-corpus (i18n/tr ui "Unknown corpus")
    :rejected       (i18n/tr ui "Left out of the search")
    :misaligned     (i18n/tr ui "Unreadable CQP output")
    :internal       (i18n/tr ui "Unexpected error")
    (i18n/tr ui "CQP error")))

(defn error-explanation
  "What the error of `type` means, in `ui`, for the types that carry no
  message of their own; nil for the rest, whose message says it."
  [ui type]
  (case type
    :no-corpus      (i18n/tr ui "Select at least one corpus to search.")
    :no-texts       (i18n/tr ui (str "Without a text attribute there is "
                                     "nothing to read as one text."))
    :unknown-corpus (i18n/tr ui "The registry has no corpus with that name.")
    :misaligned     (i18n/tr ui "CQP did not print the requested rows.")
    :internal       (i18n/tr ui (str "The search failed on the server. The "
                                     "server log has the details."))
    nil))

(defn rejected-attrs
  "The attribute names a :rejected `error` carries, from the one or the
  several its guard named."
  [{:keys [attr attrs] :as error}]
  (map name (or (seq attrs) (when attr [attr]))))

(defn rejection-explanation
  "Why one of this project's own guards left the `n` corpora an `error`
  concerns out of the search, in `ui`: worded from its :reason and the
  attributes it named. Nil for a guard naming neither, whose message is
  ours rather than the reader's and says nothing they can act on."
  [ui {:keys [reason] :as error} n]
  ;; every sentence spelled out at its own call: the scanner reads the
  ;; literal arguments of `trn`, and a helper passing them on hides them
  (when-let [attr (not-empty (str/join ", " (rejected-attrs error)))]
    (let [values {:attr attr}]
      (case reason
        :not-groupable
        (i18n/trn ui
                  "The corpus cannot be counted by {attr}."
                  "The corpora cannot be counted by {attr}."
                  n values)

        :not-sortable
        (i18n/trn ui
                  "The corpus cannot be sorted by {attr}."
                  "The corpora cannot be sorted by {attr}."
                  n values)

        :no-attr
        (i18n/trn ui
                  "The corpus has no attribute {attr}."
                  "The corpora have no attribute {attr}."
                  n values)

        ;; the only guard naming several attributes, so the sentence lists
        ;; them rather than agreeing with them: `trn` counts the corpora
        :no-filter-attr
        (i18n/trn ui
                  "The corpus has no metadata field of that name: {attr}."
                  "The corpora have no metadata field of that name: {attr}."
                  n values)

        :too-many-values
        (i18n/trn ui
                  "The corpus has too many values of {attr} to search a range."
                  "The corpora have too many values of {attr} to search a range."
                  n values)

        nil))))

(defn bare-word-error?
  "True when CQP's error `message` is the one a bare word in a CQP query
  gets: it is read as the name of a corpus or a query result, and
  refused as one there is none of."
  [message]
  (boolean (re-find #"Corpus ``.*'' is undefined" (str message))))

(defn error-body
  "The parts of an `error` under its heading in `ui`: the `corpora` it
  concerns, the explanation of a type that carries no message, why a
  guard of ours refused, what a bare word in CQP gets told, and CQP's
  own message verbatim as the sample output of another program."
  [ui {:keys [type message] :as error} corpora]
  (list
   (when (seq corpora)
     [:p (str (i18n/tr ui "in") " ")
      (interpose ", " (map (fn [c] [:code c]) corpora))])
   (when-let [explanation (error-explanation ui type)]
     [:p explanation])
   (when-let [explanation (rejection-explanation ui error (count corpora))]
     [:p explanation])
   (when (bare-word-error? message)
     [:p (i18n/tr ui (str "CQP reads a bare word as the name of a query "
                          "result. To match a word, put it in quotation "
                          "marks."))])
   ;; the stylesheet scrolls this rather than letting cqp's column-aligned
   ;; pointer reflow, and a scroll container a keyboard cannot reach is
   ;; unreadable in the browsers that do not focus scrollers themselves.
   ;; Only CQP's own words go in it: a guard of ours is untranslated
   ;; English that never was another program's output
   (when (and message (not= :rejected type))
     [:pre {:tabindex "0"} [:samp message]])))

(defn cqp-error-section
  "An `error` map as an error section in `ui`, naming the `corpora` it
  concerns when given."
  [ui error corpora]
  (widgets/error-section (error-heading ui error)
                         (error-body ui error corpora)))

(defn result-heading
  "The heading naming the results region in `ui`: what the `result` of
  the search `params` describe found when any corpus could be searched,
  else the name of the `error` that came instead, so a search that
  failed everywhere is not announced as a count of nothing."
  [ui params {:keys [counts size] :as result} error]
  (if (searched? result)
    (hits-heading ui params size (counting? result))
    (error-heading ui (or error (some :error counts)))))

(defn results-region
  "The outcome of a search in `state` under `heading`, as a region named
  by that heading and focusable, so a GET search can land on it: a
  header of the heading and the `subheading` phrases; a status line
  while the result is still being counted; the errors; then `body`, the
  view's own content. The switch between the views stands on the query
  line instead (see dk.cst.corpus-probe.views/search-page)."
  [{:keys [ui result error pending?] :as state}
   heading subheading body]
  ;; named by the heading alone: a screen reader landing here hears the
  ;; count, not the whole question, which the controls below restate
  [:section.result (cond-> {:id              url/results-id
                            :tabindex        "-1"
                            :aria-labelledby "results-heading"}
                     ;; while the next question is in flight these hits
                     ;; are still the previous one's answer, and nothing
                     ;; about them says so
                     pending? (assoc :aria-busy "true"))
   [:header.result-head
    [:hgroup
     ;; the page's h1: the search page has no other, so what a search
     ;; found is what the page is about
     [:h1 {:id "results-heading"} heading]
     (when-let [phrases (seq (remove nil? (cons (question ui state)
                                                subheading)))]
       [:p (interpose " · " phrases)])]]
   ;; always rendered, and before anything whose kind can change (see
   ;; dk.cst.corpus-probe.views.widgets/status)
   (widgets/status
    (when (counting? result)
      [:p (str (i18n/tr ui "Counting hits in") " "
               (corpora-phrase ui (:remaining result)) " …")]))
   (when error (error-body ui error nil))
   (for [[e corpora] (error-groups (:counts result))]
     (cqp-error-section ui e corpora))
   body])
