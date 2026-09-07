(ns dk.cst.corpus-probe.views.result
  "What both views of a search result share: the results region with its
  heading, the phrases under the heading naming what was asked, the
  switch between the views, the controls over a result and the pager,
  the download links, and the CQP error vocabulary, rendered as sections
  headed by their names.

  The markup uses the element HTML provides for each part: a named
  region for the outcome, <nav> for pagination and for the switch,
  headings for errors, so the document is meaningful without the
  stylesheet. Nothing here knows the search form: the region answers the
  params a search was asked with, which the form may have moved on from."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.tokens :as tokens]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(defn query-phrase
  "The query of `params` in words for a title, in `ui`: the text as
  typed, or, for a list, how many words it holds (see
  dk.cst.corpus-probe.query/words), a title being one line and a list
  not, or, for an extended search, the CQP its tokens compile to (see
  dk.cst.corpus-probe.query/->cqp), which is what the CQP mode shows
  too."
  [ui {:keys [q] :as params}]
  (case (mode/mode params)
    "list"     (let [n (count (query/words q))]
                 (str n " " (i18n/trn ui "word" "words" n)))
    "extended" (query/->cqp (query/of params))
    q))

(defn form-query
  "The query the search form holds (see dk.cst.corpus-probe.query/of):
  that of its `params`, or, in the extended mode, of its `tokens` as the
  client keeps them (see dk.cst.corpus-probe.query.tokens/rows->params),
  kept within the unit the params name."
  [params tokens]
  (let [mode (mode/mode params)]
    (query/of (if (= "extended" mode)
                (assoc (tokens/rows->params tokens)
                       :mode mode :within (:within params))
                params))))

(defn match-label
  "What the `match` param value is called, in `ui`: how much of the
  form a simple search must cover (see
  dk.cst.corpus-probe.query.params/match-op); the whole word for a value
  naming none."
  [ui match]
  (case match
    "prefix" (i18n/tr ui "start of word")
    "suffix" (i18n/tr ui "end of word")
    "infix"  (i18n/tr ui "part of word")
    (i18n/tr ui "whole word")))

(defn asked?
  "True when the search `params` ask for anything (see
  dk.cst.corpus-probe.query/of). A search asking nothing counts every
  token, which only a frequency table wants."
  [params]
  (some? (query/of params)))

(defn counting?
  "True while the corpora of `result` are still being counted: some of
  them are `:remaining` (see dk.cst.corpus-probe.search/concordance!),
  and its size is the hits counted so far."
  [{:keys [remaining] :as result}]
  (boolean (seq remaining)))

(defn searched?
  "True when any corpus of `result` could be searched, or is still being
  counted (see `counting?`), so its counts are an answer rather than a
  report of failure."
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
  nothing was `asked?` of `params`. Not the query: the field above the
  answer holds it (see `question` for when it does not)."
  ([ui params size]
   (hits-heading ui params size false))
  ([ui params size counting?]
   (if (asked? params)
     (str (when counting? (str (i18n/tr ui "at least") " "))
          (hits-phrase ui size))
     (i18n/tr ui "All tokens"))))

(defn query-mark
  "The query of `params` as the answer names it, in `ui`: a CQP query as
  the code it is, and so an extended search, as the CQP it compiles to
  (see `query-phrase`), a list as how many words it holds, and a simple
  search quoted, being a word spoken of rather than used."
  [ui params]
  (let [phrase (query-phrase ui params)]
    (case (mode/mode params)
      ("cqp" "extended") [:code phrase]
      "list"             phrase
      [:q phrase])))

(defn question
  "The query the result of `state` answered, named as `query-mark` names
  it, once the form above has moved on from it: retyped, or switched to
  a mode that could not keep it, so that the answer still says what it
  is of. Its `:asked` params are what ran (see
  dk.cst.corpus-probe.server.search/search-view-data); the form's query
  is read
  from its `:params` and `:tokens` (see `form-query`). Nil while the
  form holds the query, which then says it, and the answer names only
  how many."
  [ui {:keys [asked params tokens]}]
  (when (not= (query/of asked) (form-query params tokens))
    (query-mark ui asked)))

(defn corpora-phrase
  "The corpus `names` in words in `ui`: the one name, or how
  many there were."
  [ui names]
  (if (= 1 (count names))
    (first names)
    (str (count names) " " (i18n/tr ui "corpora"))))

(defn page-phrase
  "Where in a paged `result` the reader is, in `ui`: the page, and of how
  many once the result is counted (see `counting?`)."
  [ui {:keys [page pages] :as result}]
  (str (i18n/tr ui "page") " " (inc page)
       (when pages (str " " (i18n/tr ui "of") " " pages))))

(defn sample-phrase
  "That a result holds a random `sample` of the matches rather than all
  of them, in `ui`, over `corpora`; nil when it holds them all.

  The number is the sample asked for rather than the hits it came back
  with, the two differing wherever a corpus had fewer matches than that,
  and it is named as being per corpus over several, one sample being
  drawn in each (see dk.cst.corpus-probe.search/concordance!)."
  [ui sample corpora]
  (when sample
    (str (i18n/tr ui "a random sample of at most") " "
         (i18n/group-digits ui sample)
         (when (next corpora) (str " " (i18n/tr ui "per corpus"))))))

(defn position-label
  "What the `position` of a match (see
  dk.cst.corpus-probe.cwb.command/positions) is called, in `ui`, worded to
  follow an attribute name; the position itself for one nothing names."
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
  `subset` has its :value as its :attr, in `ui`, the attribute and the
  value as the code they are; nil without one."
  [ui {:keys [anchor attr value] :as subset}]
  (when subset
    (list [:code (name attr)] " " (position-label ui anchor) " = "
          [:code value])))

(defn near-phrase
  "That a result holds only the hits with the :word of `near` nearby,
  in `ui`; nil without one."
  [ui {:keys [word] :as near}]
  (when near
    (list (i18n/tr ui "near") " " [:code word])))

(defn filter-phrase
  "The metadata `filter` (a map of attribute to the set of values
  accepted) and the `patterns` beside it (a map of attribute to the
  regexes accepted) in words: each attribute with its values, sorted,
  then its patterns between slashes; empty without either.

  (filter-phrase {:text_year #{\"1591\" \"1583\"}} {:text_title [\"Hav.*\"]})
  ;; => \"text_title /Hav.*/; text_year 1583, 1591\""
  [filter patterns]
  (str/join "; " (for [attr (sort (distinct (concat (keys filter)
                                                    (keys patterns))))]
                   (str (name attr) " "
                        (str/join ", " (concat (sort (get filter attr))
                                               (map #(str "/" % "/")
                                                    (get patterns attr))))))))

(defn within-phrase
  "The metadata `filter` and its `patterns` as a qualifier of a result
  in `ui`, or nil without either: \"within text_year 1591\"."
  [ui filter patterns]
  (when (or (seq filter) (seq patterns))
    (str (i18n/tr ui "within") " " (filter-phrase filter patterns))))

(defn qualifiers
  "The question a `result` answered, less the query itself, as short
  phrases in `ui`, each naming what one control holds: the attribute a
  simple search of `params` matched and the part of the form, when not
  the usual ones and when the mode read them (see
  dk.cst.corpus-probe.query.mode/reads?); the corpora searched, those still
  being counted among them; the metadata filter; the narrowings; the
  sample. For the line under the heading (see `results-region`), where
  the heading says what was found and this what was asked."
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
             (within-phrase ui (:filter result) (:patterns result))
             (subset-phrase ui (:subset result))
             (near-phrase ui (:near result))
             ;; a search that found nothing sampled nothing, and saying it
             ;; drew a sample of what it found reads as the reason it is
             ;; empty
             (when (pos? (or size 0))
               (sample-phrase ui sample searched))])))

(defn view-label
  "What the result view `k` is called, in `ui` (see
  dk.cst.corpus-probe.url/result-views): the concordance is KWIC, as
  CWB and KORP call it, expanded but not linked, since the label is
  itself a link."
  [ui k]
  (case k
    :kwic        (widgets/term ui :kwic false)
    :frequencies (i18n/tr ui "Frequencies")
    (name k)))

(defn view-switch
  "The switch between the views of one result in `ui`: each of
  `hrefs` ([view url], see dk.cst.corpus-probe.url/view-hrefs) as a
  link named by `view-label`, `view` marked as the one being shown; nil
  without hrefs.

  Links rather than an ARIA tablist: each view is its own URL and its own
  question put to CQP, so following one is a navigation, which is what a
  link means. A tablist would promise a panel that is already loaded."
  [ui view hrefs]
  (when (seq hrefs)
    [:nav.views.menu {:aria-label (i18n/tr ui "Result view")}
     (widgets/link-row (for [[k href] hrefs] [k href (view-label ui k)])
                       view)]))

(defn apply-button
  "The button applying a result's controls where no `client?` runs to
  apply them itself, in `ui`; nil where one does.

  Each control applies itself on being changed where the client runs, so
  there is no button: choosing an order is asking for it, and a control
  that needs a second control to take effect is one the reader has to be
  told about. Without a client nothing can act on a change, so the button
  is what applies it there.

  Inside <noscript>: the server renders every page for the reader without
  a script, and a browser with one showed the button for the split second
  before the client's first render took it away. Wrapped, that browser
  never shows it, while one without a script does."
  [ui client?]
  (when-not client?
    (list " " [:noscript
               [:button {:type "submit" :form url/form-id}
                (i18n/tr ui "Apply")]])))

(defn view-controls
  "The controls of a result in `ui`: the `reading` ones (hiccup), which
  decide how the hits are read rather than what was searched for, and
  behind a disclosure the `narrowing` ones (hiccup; nil for none), which
  keep only some of the hits, `open?` saying whether that disclosure
  starts open. Nil without either.

  They live with the result rather than in the query form, so re-ordering
  a concordance costs a click instead of a scroll back past the form.
  Narrowing one is here for the same reason though it runs the query
  again: which of the hits to keep is a question the reader has on
  seeing them. It is behind a disclosure because it is the rarer
  question, and a row of six controls reads as a form to fill in; the
  disclosure is open whenever a narrowing is in force, so what narrows a
  result is never hidden from the reader it narrows it for.

  Each row carries the `apply-button` where no `client?` runs."
  [ui client? reading narrowing open?]
  (when (or reading narrowing)
    [:div.view-controls
     (when reading
       [:p reading (apply-button ui client?)])
     (when narrowing
       [:details {:open (boolean open?)}
        [:summary (i18n/tr ui "Narrow the result")]
        [:p narrowing (apply-button ui client?)]])]))

(defn pager-links
  "The page links of a result around `position` (where in the sequence
  the reader is), labelled in `ui` (see
  dk.cst.corpus-probe.views.widgets/pager); nil when neither `prev-href`
  nor `next-href` is in range."
  [ui prev-href next-href position]
  (widgets/pager (when prev-href
                   [prev-href (str "← " (i18n/tr ui "previous"))])
                 (when next-href
                   [next-href (str (i18n/tr ui "next") " →")])
                 position))

(defn pagination
  "`pager-links` as a navigation landmark named in `ui`.

  Only one of a result's two pagers is a landmark: the APG asks each
  landmark of a repeated role to carry a name of its own, and two named
  Pagination cannot be told apart. The repeat below the table is the bare
  list, whose links stay operable and stay in the tab order."
  [ui prev-href next-href position]
  (when-let [links (pager-links ui prev-href next-href position)]
    [:nav.pagination.menu {:aria-label (i18n/tr ui "Pagination")} links]))

(defn download-links
  "Links downloading the current table in each format of `hrefs` (format
  keyword to URL), with `note` (when given) qualifying what the download
  holds; nil without hrefs, worded in `ui`. The response itself
  asks to be saved (its Content-Disposition), so the links carry no
  download attribute."
  [ui hrefs note]
  (when (seq hrefs)
    [:p.downloads (i18n/tr ui "Download")
     (when note (str " " note))
     ": "
     (interpose " · "
                (for [[format href] (sort hrefs)]
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
  as such and carries its message.

  TODO: a stored result read back damaged (see
  dk.cst.corpus-probe.search.result/read-stored!) is headed as CQP output
  that could not be read, which it is; it may want words of its own."
  [ui {:keys [type] :as error}]
  (case type
    :timeout        (i18n/tr ui "The search did not finish in time")
    :no-corpus      (i18n/tr ui "No corpus selected")
    :no-texts       (i18n/tr ui "The corpus marks no texts")
    :unknown-corpus (i18n/tr ui "Unknown corpus")
    :rejected       (i18n/tr ui "Request rejected")
    (:misaligned :damaged) (i18n/tr ui "Unreadable CQP output")
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

(defn bare-word-error?
  "True when CQP's error `message` is the one a bare word in a CQP query
  gets: it is read as the name of a corpus or a query result, and
  refused as one there is none of."
  [message]
  (boolean (re-find #"Corpus ``.*'' is undefined" (str message))))

(defn error-body
  "The parts of an `error` under its heading in `ui`: the
  `corpora` it concerns, the explanation of a type that carries no message,
  what a bare word in CQP gets told (see `bare-word-error?`), and cqp's
  own message verbatim, its `<--` position pointer included, as the
  sample output of another program."
  [ui {:keys [type message]} corpora]
  (list
   (when (seq corpora)
     [:p (str (i18n/tr ui "in") " ")
      (interpose ", " (map (fn [c] [:code c]) corpora))])
   (when-let [explanation (error-explanation ui type)]
     [:p explanation])
   (when (bare-word-error? message)
     [:p (i18n/tr ui (str "CQP reads a bare word as the name of a query "
                          "result. To match a word, put it in quotation "
                          "marks."))])
   ;; the stylesheet scrolls this rather than letting cqp's column-aligned
   ;; pointer reflow, and a scroll container a keyboard cannot reach is
   ;; unreadable in the browsers that do not focus scrollers themselves
   (when message [:pre {:tabindex "0"} [:samp message]])))

(defn cqp-error-section
  "An `error` map under a heading of its own in `ui` (see
  dk.cst.corpus-probe.views.widgets/error-section), naming the `corpora`
  it concerns when given. The reader reaches the error because the
  search lands on it (see `results-region`)."
  [ui error corpora]
  (widgets/error-section (error-heading ui error)
                         (error-body ui error corpora)))

(defn result-heading
  "The heading naming the results region in `ui`: what the concordance
  `result` of the search `params` describe found (see `hits-heading`)
  when any corpus could be searched, else the name of the error that
  came instead, so a search that failed everywhere is not announced as a
  count of nothing."
  [ui params {:keys [counts size] :as result} error]
  (if (searched? result)
    (hits-heading ui params size (counting? result))
    (error-heading ui (or error (some :error counts)))))

(defn results-region
  "The outcome of a search in `state` under `heading`, as a region named by
  that heading and focusable, so a GET search can land on it.

  Every view of a result shares this: a header holding the heading, the
  `subheading` phrases (see `qualifiers`) under it and the switch
  between the views at its end; a status line while the result is still
  being counted (see `counting?`); the error that replaced the result or
  the errors of individual corpora; and then `body`, the view's own
  content. The two views differ only in what they say about the same
  hits, so they differ only in what they pass here.

  The heading is the page's h1: the search page has no other, so what a
  search found, or why it found nothing, is what the page is about. It
  is the answer alone, how many: the query is in the field above, which
  is the page's headline, and the answer repeats it only once the form
  has moved on from it (see `question`), at the head of the line under
  the heading. That line is the rest of the question, grouped with the
  heading as heading and subheading, and the region is named by the
  heading alone, so a screen reader landing here hears the count and not
  the whole question, which the controls below restate anyway.

  Marked busy while a navigation is `pending?`, since until that one
  lands what this holds is the answer to the question before it."
  [{:keys [ui view view-hrefs result error pending?] :as state}
   heading subheading body]
  [:section.result (cond-> {:id              url/results-id
                            :tabindex        "-1"
                            :aria-labelledby "results-heading"}
                     ;; while the next question is in flight these hits
                     ;; are still the previous one's answer, and nothing
                     ;; about them says so
                     pending? (assoc :aria-busy "true"))
   [:header.result-head
    [:hgroup
     [:h1 {:id "results-heading"} heading]
     (when-let [phrases (seq (remove nil? (cons (question ui state)
                                                subheading)))]
       [:p (interpose " · " phrases)])]
    (view-switch ui view view-hrefs)]
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
