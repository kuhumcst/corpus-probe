(ns dk.cst.corpus-probe.views.concordance
  "Hiccup for the KWIC concordance: the table of hits, the controls that
  resubmit it, the concordance view of a result and the panel inspecting
  one of its tokens. The pure cursor arithmetic the client moves focus
  with lives here too.

  Tokens carry their annotations as `data-*` attributes and their surface
  form as text; the match is a `<mark>`."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.result :as result]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(def column-count
  "How many columns a concordance row has, which a full-width row spans."
  4)

(defn hit-key
  "The key identifying `hit` in a concordance over several corpora: its
  corpus and its corpus position, since positions repeat across corpora."
  [{:keys [corpus cpos] :as hit}]
  [corpus cpos])

(defn hit-of
  "The hit among `hits` with key `k` (see `hit-key`); nil for none."
  [hits k]
  (first (filter #(= k (hit-key %)) hits)))

(defn token-title
  "Tooltip text for token map `m`: its non-word attributes joined by ' · ';
  nil for a corpus that annotates nothing, which has no tooltip to show."
  [m]
  (->> (dissoc m :word :open :close)
       (vals)
       (remove str/blank?)
       (str/join " · ")
       (not-empty)))

(defn token-data
  "The annotations of token `m` as `data-*` attributes, one per positional
  attribute except the surface `:word` (the element's text) and the
  structure tags."
  [m]
  (into {} (for [[k v] (dissoc m :word :open :close)]
             [(keyword (str "data-" (name k))) v])))

(defn token-count
  "How many tokens `hit` shows, across its three columns: the length of
  the run the cursor moves along."
  [{:keys [left match right] :as hit}]
  (+ (count left) (count match) (count right)))

(def fade-steps
  "Over how many words the context past the width asked for falls away,
  the last of them out of sight: the page holds far more than it shows
  (see dk.cst.corpus-probe.search.batch/fetch-context), and a line that
  ended in a wall of grey would read as the answer rather than as the
  way on."
  3)

(defn faded-tokens
  "How far outside the reader's window each token of `hit` falls: token
  index to a step from 1, the first word past the window, to
  `fade-steps`. The window is the `context` words either side of the
  match, moved along the line by `travel` words, so that a step of the
  cursor uncovers one word ahead and lets one behind fall away. Empty
  for a unit of text, which bounds itself."
  [{:keys [left match] :as hit} context travel]
  (if-not (number? context)
    {}
    (let [nl   (count left)
          nm   (count match)
          ;; the last token before the window and the first one after it
          upto (max 0 (- nl context (- travel)))
          from (+ nl nm context travel)
          step (fn [out] (min fade-steps (max 1 out)))]
      (into {}
            (concat (for [i (range (min nl upto))]
                      [i (step (- upto i))])
                    (for [i (range (max (+ nl nm) from) (token-count hit))]
                      [i (step (inc (- i from)))]))))))

(defn anchored-tokens
  "Which tokens of `hit` its target and keyword anchors fall on: token
  index to anchor name, for each anchor set and within the row."
  [{:keys [cpos left anchors] :as hit}]
  (into {}
        (for [k     [:target :keyword]
              :let  [i (some-> (get anchors k) (- cpos) (+ (count left)))]
              :when (and i (< -1 i (token-count hit)))]
          [i k])))

(defn token->offset
  "How far token `i` of `hit` is from the start of its match: negative in
  the left context, zero at the first token of the match, positive after
  it.

  This is what lines a hit up with its wider context, the same match with
  more text around it, so moving between them lands on the same word
  rather than on the same column."
  [hit i]
  (- i (count (:left hit))))

(defn offset->token
  "The index of the token of `hit` at `offset` from its match, or the
  nearest one when the offset falls outside the row: a narrow row cannot
  answer an offset only a wide one has."
  [hit offset]
  (min (dec (token-count hit))
       (max 0 (+ offset (count (:left hit))))))

(defn default-cursor
  "The cursor for `hits` when nothing has moved it yet: the match of the
  first hit, so exactly one token is tabbable and the concordance is one
  tab stop rather than hundreds. The match rather than the first token,
  which is the far end of the context and out of sight (see
  `faded-tokens`), and which would put the reader's window there."
  [hits]
  (when-let [hit (first hits)]
    [(hit-key hit) (count (:left hit))]))

(defn resolved-cursor
  "The `cursor` where it names a token among `hits`, else the
  `default-cursor`: one left behind by a page that has since narrowed
  would leave the concordance with no tab stop at all."
  [hits [k i :as cursor]]
  (or (when-let [hit (hit-of hits k)]
        (when (< i (token-count hit)) cursor))
      (default-cursor hits)))

(defn travel-offset
  "How far along the line from its match the reader has moved, in words:
  the offset of the token the `cursor` is on (see `token->offset`), which
  every row's window follows so the page travels as one. Zero when the
  cursor is on no hit here."
  [hits [k i]]
  (if-let [hit (hit-of hits k)]
    (token->offset hit i)
    0))

(defn token-id
  "The id of token `i` of `hit`, so the client can move focus to it."
  [hit i]
  (str "t-" (:corpus hit) "-" (:cpos hit) "-" i))

(defn cursor-id
  "The id of the token `cursor` is on (see `token-id`), which focus and
  the concordance's scroll both follow; nil for no cursor."
  [[[corpus cpos] i :as cursor]]
  (when cursor
    (token-id {:corpus corpus :cpos cpos} i)))

(defn anchor-class
  "The class marking the token an `anchor` falls on, as cqp marks a
  target in bold and a keyword underlined (manual section 3.3)."
  [anchor]
  (case anchor
    :target  "target"
    :keyword "keyword"))

(defn token
  "Token `i` of `hit`, the map `m`, under the concordance `opts` (see
  `concordance`): its surface form as text, its annotations as `data-*`
  attributes, and focus or a click inspecting it along with `source`, the
  :corpus and :structs of its hit. A `<button>` under `:client?`, a plain
  span otherwise.

  Only the token at `:cursor` is tabbable, the arrow keys moving the
  cursor between neighbours, as the APG asks of a grid of controls."
  [{:keys [client? cursor anchored faded] :as opts} hit source i m]
  (let [k       [(hit-key hit) i]
        inspect [:inspect (assoc source :token m) k]
        anchor  (get anchored i)
        title   (->> [(some-> anchor name) (token-title m)]
                     (remove nil?)
                     (str/join " · ")
                     (not-empty))
        out     (get faded i)
        classes (cond-> []
                  anchor (conj (anchor-class anchor))
                  out    (conj "faded" (str "fade-" out)))
        attrs   (cond-> (token-data m)
                  title         (assoc :title title)
                  (seq classes) (assoc :class classes))]
    (if-not client?
      ;; no handler: nothing answers a click here, and the string renderer
      ;; would drop one anyway
      [:span.token attrs (:word m)]
      [:button.token
       (assoc attrs
              :type     "button"
              :id       (token-id hit i)
              :tabindex (if (= cursor k) "0" "-1")
              ;; inspecting follows focus rather than waiting for a press,
              ;; so moving the cursor moves what the panel describes
              :on       {:focus   inspect
                         :keydown [:move-cursor k :event/key :event/ctrl?]
                         :click   inspect})
       (:word m)])))

(defn tokens
  "The token maps `ms` of `hit` from `source` (its :corpus and :structs)
  under the concordance `opts`, numbered from `offset` within the hit, as
  elements separated by spaces."
  [opts hit source offset ms]
  (interpose " " (map-indexed (fn [i m] (token opts hit source (+ offset i) m))
                              ms)))

(defn structs-title
  "Tooltip text listing the structural annotations `structs` of a hit;
  nil for a corpus that marks none, which has no tooltip to show."
  [structs]
  (->> structs
       (map (fn [[k v]] (str (name k) ": " v)))
       (str/join "\n")
       (not-empty)))

(defn position-data
  "The hit's corpus positions as `data-*` attributes: the match start
  (`cpos`) and end, plus the target and keyword anchors when set."
  [cpos {:keys [matchend target keyword]}]
  (cond-> {:data-cpos (str cpos)}
    matchend (assoc :data-matchend (str matchend))
    target   (assoc :data-target (str target))
    keyword  (assoc :data-keyword (str keyword))))

(defn hit-source
  "The source of `hit` that its tokens are inspected with: its :corpus,
  its structural metadata :structs and its positions, :cpos and
  :matchend, which the reading page of its text takes."
  [hit]
  (assoc (select-keys hit [:corpus :structs :cpos])
         :matchend (:matchend (:anchors hit))))

(defn position-cell
  "The corpus position of `source` (see `hit-source`) as the head of its
  row: a link to the reading page of its text with the hit marked, which
  is where the whole of the context is, named by the hit's structural
  annotations (see `structs-title`). The bare position for a hit that
  knows no corpus, which there is no text page for."
  [{:keys [corpus structs cpos matchend] :as source}]
  (let [title (structs-title structs)]
    [:th.kwic-cpos (cond-> {:scope "row"}
                     title (assoc :title title))
     (if corpus
       [:a {:href (url/text corpus cpos matchend)} (str cpos)]
       (str cpos))]))

(defn hit-row
  "One KWIC `hit` as a table row under the concordance `opts`, with its
  corpus positions, its anchored tokens marked (see `anchored-tokens`)
  and the ones outside the reader's window faded (see `faded-tokens`)."
  [{:keys [context travel] :or {travel 0} :as opts} hit]
  (let [source (hit-source hit)
        opts   (assoc opts
                      :anchored (anchored-tokens hit)
                      :faded    (faded-tokens hit context travel))
        {:keys [left match right anchors cpos]} hit
        nl     (count left)]
    [:tr.kwic-hit (position-data cpos anchors)
     ;; the position heads the row, so every other cell resolves a row
     ;; header as well as a column one
     (position-cell source)
     [:td.kwic-left (tokens opts hit source 0 left)]
     [:td.kwic-match [:mark (tokens opts hit source nl match)]]
     [:td.kwic-right (tokens opts hit source (+ nl (count match)) right)]]))

(defn corpus-group
  "The rows of `hits`, all from one corpus, as a row group under the
  concordance `opts` (see `concordance`): a header row naming the corpus
  and, from the per-corpus `:counts` of the search, how many hits it
  holds in all, then the hit rows. A corpus whose query failed has no
  count.

  The count is of the whole corpus, not of the rows below it."
  [{:keys [ui langs counts] :as opts} [{:keys [corpus]} :as hits]]
  (let [corpus-lang (get langs corpus)
        size        (some #(when (= corpus (:corpus %)) (:size %)) counts)]
    [:tbody (cond-> {}
              corpus      (assoc :data-corpus corpus)
              corpus-lang (assoc :lang corpus-lang))
     (when corpus
       [:tr.kwic-corpus
        [:th {:scope "rowgroup" :colspan column-count}
         ;; on a box of its own, so it keeps to the start of the region
         ;; while the rows below it travel sideways
         [:span.pinned
          [:a {:href (url/corpus corpus)}
           [:code corpus]]
          (when size
            (list " " (widgets/count-badge (i18n/group-digits ui size))))]]])
     (map (partial hit-row opts) hits)]))

(defn context-heading
  "A context column's heading: `arrow`, which points away from the match
  and into the context, with `s` naming the column to a reader who hears
  the table and to a pointer resting on the arrow."
  [arrow s]
  ;; the title is hidden with the arrow that carries it, so the name is
  ;; spoken the once, as the heading, and never again as a description
  (list [:span.spoken s]
        [:span.kwic-arrow {:aria-hidden "true" :title s} arrow]))

(defn column-headers
  "The concordance's column headings in `ui`."
  [ui]
  ;; each heading carries the class of its column, so a rule about a
  ;; column also reaches its heading
  [:thead
   [:tr
    [:th.kwic-cpos {:scope "col"} (widgets/term ui :cpos false)]
    [:th.kwic-left {:scope "col"}
     (context-heading "←" (i18n/tr ui "left context"))]
    [:th.kwic-match {:scope "col"} (widgets/term ui :match false)]
    [:th.kwic-right {:scope "col"}
     (context-heading "→" (i18n/tr ui "right context"))]]])

(defn context-value
  "The context width `context` as a URL param and as an attribute value.
  A number of words stays a number. A unit of text becomes its name."
  [context]
  (if (keyword? context) (name context) context))

(def caption-id
  "The id of the concordance's caption, which names its scroll region."
  "concordance-caption")

(defn caption
  "What the concordance is called, in `ui`: the term, and which page of
  `result` it holds where there is more than one (`paged?`).

  The pager stands under the table, so a reader who hears the page rather
  than seeing it would otherwise meet the rows before the page they are
  on (see dk.cst.corpus-probe.views.result/page-phrase)."
  [ui result paged?]
  (cond-> (widgets/term ui :kwic false)
    paged? (list " · " (result/page-phrase ui result))))

(def region-id
  "The id of the region the concordance scrolls in. The client focuses it
  by this name rather than by the class the stylesheet uses, so renaming
  a style hook cannot break focus."
  "concordance")

(def keys-id
  "The id of the concordance's keyboard instructions, which describe its
  region (see `key-help`)."
  "concordance-keys")

(defn key-help
  "How the concordance is read by keyboard, in `ui`: spoken, never seen,
  since a reader who can see the cursor move needs no telling.

  The table keeps its own semantics rather than taking the grid roles of
  the APG pattern, which would cost a screen reader the rows and columns
  a concordance is read by. Nothing then says the arrow keys move a
  cursor through the words, so the region says it in words."
  [ui]
  [:p.spoken {:id keys-id}
   (i18n/tr ui (str "Use the arrow keys to move between the words, "
                    "Home and End for the ends of a line."))])

(defn concordance
  "The KWIC `hits` of one result page as a table, one row group per corpus
  in the order the hits arrive, inside the region that scrolls it.

  `opts` carries the `:caption` naming the table, the `:ui` of its
  headings and controls, `:langs` (corpus to the language of its text),
  the per-corpus `:counts` heading each row group, the `:context` asked
  for, which says how wide the reader's window is, `:client?` where the
  script answering a token click runs, and `:cursor`, the one tabbable
  token, which the window and the scroll both follow."
  [hits {:keys [caption ui client? context] :as opts}]
  (let [cursor (resolved-cursor hits (:cursor opts))
        opts   (assoc opts :cursor cursor :travel (travel-offset hits cursor))]
    ;; a KWIC line must not wrap, or its columns stop lining up, so the
    ;; table stands in a region that clips it and is scrolled by the
    ;; cursor alone; not a tab stop, since there is nothing to scroll by
    ;; hand, but focusable because the panel sends focus back here on
    ;; closing, and named because a focusable region needs a name
    (widgets/table-box
     (cond-> {:id              region-id
              :role            "region"
              :tabindex        "-1"
              :aria-labelledby caption-id
              ;; the panel describes what the cursor is on, so it has
              ;; nothing to describe once the cursor is left behind
              :on              {:focusout [:leave-concordance]}}
       ;; the width of line that the reader asked for. The page needs
       ;; this to set its own width (style.css, `.search-page:has(...)`)
       context
       (assoc :data-context (context-value context))
       ;; the token the reader is on stays in the middle, so the scroll
       ;; follows the cursor; the reach travels with it, since a page
       ;; that has just come back wider has grown under the reader and
       ;; must not be glided
       client?
       (assoc :replicant/on-render
              [:centre-match (cursor-id (:cursor opts)) (:reach opts)]
              ;; on the region, not on the token: the cursor would say
              ;; it again at every word
              :aria-describedby keys-id))
     ;; only where the script is running: without it there is no cursor,
     ;; and the keys the help names do nothing
     (when client? (key-help ui))
     [:table.kwic
      ;; spoken, but never seen: the view controls above name this table
      ;; on the screen. A table still needs a caption, and a region still
      ;; needs a name
      (when caption
        [:caption.spoken {:id caption-id} caption])
      (column-headers ui)
      (map (partial corpus-group opts) (partition-by :corpus hits))])))

(defn sort-label
  "What the sort mode `value` (see
  dk.cst.corpus-probe.cwb.command/sort-modes) is called, in `ui`; a mode
  naming a positional attribute is the match by that attribute."
  [ui value]
  (case value
    "corpus"  (i18n/tr ui "corpus order")
    "word"    (i18n/tr ui "match")
    "reverse" (i18n/tr ui "match from the end")
    "left"    (i18n/tr ui "left context")
    "right"   (i18n/tr ui "right context")
    "random"  (i18n/tr ui "random")
    (str (i18n/tr ui "match") " " value)))

(defn sort-control
  "The sort control of the concordance in `ui`: a select over the
  `sort-modes` values with `sort` chosen, each named by `sort-label`.

  It names the form it submits with (see
  dk.cst.corpus-probe.views.widgets/select), so it can sit beside the
  table it reorders rather than inside the query form."
  [ui sort-modes sort]
  (widgets/select url/form-id "sort" (i18n/tr ui "Sort")
                  (for [value sort-modes]
                    (widgets/option sort value (sort-label ui value)))))

(def sample-sizes
  "The sample sizes the concordance offers, in display order: as many
  hits as a reader might work through by hand. A hand-written URL may
  name any other size, which `sample-control` then shows beside these."
  [50 100 500 1000])

(defn sample-control
  "The sample control of the concordance in `ui`: a select over the
  `sample-sizes` with `shown` chosen, or the whole result when it names
  none. A size the list does not hold is offered beside them, so a URL
  naming one shows as the sample it is. It names the form it submits
  with, as `sort-control` does.

  The list is the `sample` the result holds and `shown` only the mark on
  it, so that a reader's choice, which is `shown` until the search they
  asked for arrives, cannot take an option out of the list under them:
  Replicant writes what changed in its own last hiccup, and options that
  shift leave the mark where the browser put it."
  ([ui sample]
   (sample-control ui sample sample))
  ([ui sample shown]
   ;; the form holds a size as a string, the result as the number it is
   (let [sample (cond-> sample (string? sample) parse-long)
         sizes  (sort (cond-> (set sample-sizes) sample (conj sample)))]
     (widgets/select url/form-id "sample" (i18n/tr ui "Sample")
                     (list
                      (widgets/option (or shown "") "" (i18n/tr ui "all hits"))
                      (for [n sizes]
                        (widgets/option shown n (i18n/group-digits ui n))))))))

(def context-widths
  "The widths of context the concordance offers, in display order: a few
  numbers of words, then the units of text a corpus marks (see
  dk.cst.corpus-probe.cwb.corpus/unit-attrs), one region either side. A
  hand-written URL may name any other number of words, which
  `context-control` then shows beside these."
  [5 10 20 :sentence :paragraph])

(defn context-label
  "What the context width `context` (see `context-widths`) is called, in
  `ui`."
  [ui context]
  (case context
    :sentence  (i18n/tr ui "sentence")
    :paragraph (i18n/tr ui "paragraph")
    (str context " " (i18n/trn ui "word" "words" context))))

(defn context-control
  "The context control of the concordance in `ui`: a select over the
  `context-widths` with `shown` (a number of words or a unit keyword)
  chosen, named by `context-label`. A number of words the list does not
  hold is offered among the numbers, in order. It names the form it
  submits with, as `sort-control` does.

  The list is the `context` the result holds and `shown` only the mark on
  it, for the reason `sample-control` gives."
  ([ui context]
   (context-control ui context context))
  ([ui context shown]
   ;; the form holds both a width and a unit as a string, the result holds
   ;; a number and a keyword
   (let [context (cond-> context
                   (string? context) (as-> s (or (parse-long s) (keyword s))))
         widths  (if (or (keyword? context) (some #{context} context-widths))
                   context-widths
                   (into (vec (sort (conj (filterv number? context-widths)
                                          context)))
                         (filter keyword? context-widths)))]
     (widgets/select url/form-id "context" (i18n/tr ui "Context")
                     (for [width widths]
                       (widgets/option (context-value shown)
                                      (context-value width)
                                      (context-label ui width)))))))

(defn concordance-controls
  "The controls over the concordance of `state`, for its head: how the
  hits are read, and the word they must be near. Nil where no corpus
  could be searched."
  [{:keys [ui sort-modes asked params result client?]}]
  ;; what the form holds over what the result answers: a control the
  ;; reader has changed holds their choice until the search they asked
  ;; for arrives with it (see dk.cst.corpus-probe.client.actions/act)
  (let [near (result/held-near params result)]
    (when (result/searched? result)
      (if (result/found? result)
        (result/view-controls ui client?
                              (list (sort-control ui sort-modes
                                                  (result/held params :sort
                                                               (:sort asked)))
                                    " "
                                    (context-control ui (:context result)
                                                     (result/held
                                                      params :context
                                                      (:context result)))
                                    " "
                                    (sample-control ui (:sample result)
                                                    (result/held
                                                     params :sample
                                                     (:sample result))))
                              (result/near-control ui near)
                              near)
        (result/empty-controls ui client? near)))))

(defn concordance-section
  "The concordance view of the search in `state`: its sort, context and
  sample controls, the concordance itself, the pager under it and the
  download links, worded in the state's `:ui` and wrapped in
  dk.cst.corpus-probe.views.result/results-region.

  The result answers the params the search was `:asked` with, not the
  form's `:params`, which the client's form leaves behind at a change of
  mode."
  [{:keys [ui result error langs client?
           export-hrefs export-limit prev-href next-href]
    :as state}]
  (let [{:keys [counts hits size]} result
        paged?   (boolean (or prev-href next-href))
        position (when result (result/page-control ui client? result))]
    (result/results-region
     state
     (result/result-heading ui result error)
     (concordance-controls state)
     (when (result/searched? result)
       ;; a search that found nothing has nothing to page, download or
       ;; count: the table would be a header over no rows and the exports
       ;; header-only files
       (if-not (result/found? result)
         [:p (i18n/tr ui "No hits.")]
         (list
          (concordance hits {:caption   (caption ui result paged?)
                             :ui        ui
                             :langs     langs
                             :counts    counts
                             :context   (:context result)
                             :reach     (:reach result)
                             :client?   client?
                             :cursor    (:cursor state)})
          (result/pager ui prev-href next-href position)
          ;; what to do next with these hits, so it follows them: reading
          ;; the concordance is the task, taking it elsewhere is the one
          ;; after
          (result/download-links ui export-hrefs
                                 (when (and export-limit (> size export-limit))
                                   (str (i18n/tr ui "the first") " "
                                        (i18n/group-digits ui export-limit) " "
                                        (i18n/trn ui "hit" "hits"
                                                  export-limit))))))))))

(def inspector-id
  "The id of the inspection panel, by which the client finds it rather
  than by the class the stylesheet uses, as with `region-id`."
  "inspector")

(defn detail-group
  "A titled group of attributes `m` in the inspector, a box named `title`
  on its border like the fieldsets, or nil when empty."
  [title m]
  (when (seq m)
    [:section.box
     [:h3 title]
     (widgets/facts m)]))

(defn inspector
  "The token inspection panel: what the concordance's cursor is on, in
  `ui`, from `selected` (its :token, :structs, :corpus and the :cpos and
  :matchend of its hit, which the link to the whole text takes); nil
  while nothing is selected. The group titles are in `ui`, the attribute
  names inside them the corpus's own."
  [ui {:keys [token structs corpus cpos matchend] :as selected}]
  (when selected
    ;; not a popover: that would want focus and the top layer, while the
    ;; cursor must stay on the token for the arrow keys to keep moving.
    ;; Focus leaving it is how the client knows to close it
    [:aside.inspector {:id         inspector-id
                       :aria-label (i18n/tr ui "Token details")
                       :tabindex   "-1"
                       :on         {:focusout [:leave-concordance]}}
     [:h2 (i18n/tr ui "Token details")]
     [:button.inspector-close {:type "button" :on {:click [:close]}}
      (i18n/tr ui "Close")]
     (detail-group (i18n/tr ui "Token") (dissoc token :open :close))
     (detail-group (i18n/tr ui "Text") structs)
     (when (and corpus cpos)
       [:p [:a {:href (url/text corpus cpos matchend)}
            (i18n/tr ui "Read the whole text")]])
     (when corpus
       [:section.box
        [:h3 (i18n/tr ui "Corpus")]
        [:p [:a {:href (url/corpus corpus)} [:code corpus]]]])]))
