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
  5)

(def loading
  "Marks an expansion whose context is still in flight (see `hit-rows`)."
  ::loading)

(def failed
  "Marks an expansion whose context could not be fetched, as `loading`
  marks one still in flight."
  ::failed)

(defn hit-key
  "The key identifying `hit` in a concordance over several corpora: its
  corpus and its corpus position, since positions repeat across corpora."
  [{:keys [corpus cpos] :as hit}]
  [corpus cpos])

(defn context-id
  "The id of the row holding `hit`'s wider context, so the control that
  reveals it can name what it controls."
  [hit]
  (let [[corpus cpos] (hit-key hit)]
    (str "context-" corpus "-" cpos)))

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

(defn cursor-range
  "How many tokens the cursor can visit at hit key `k` among `hits`,
  given the `expanded` map: the hit's own tokens, plus its wider
  context's when one is showing."
  [hits expanded k]
  (when-let [hit (first (filter #(= k (hit-key %)) hits))]
    (+ (token-count hit)
       (let [ex (get expanded k)]
         (if (map? ex) (token-count ex) 0)))))

(defn default-cursor
  "The cursor for `hits` when nothing has moved it yet: the first token of
  the first hit, so exactly one token is tabbable and the concordance is
  one tab stop rather than hundreds."
  [hits]
  (when-let [hit (first hits)]
    [(hit-key hit) 0]))

(defn token-id
  "The id of token `i` of `hit`, so the client can move focus to it."
  [hit i]
  (str "t-" (:corpus hit) "-" (:cpos hit) "-" i))

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
  [{:keys [client? cursor anchored] :as opts} hit source i m]
  (let [k       [(hit-key hit) i]
        inspect [:inspect (assoc source :token m)]
        anchor  (get anchored i)
        title   (->> [(some-> anchor name) (token-title m)]
                     (remove nil?)
                     (str/join " · ")
                     (not-empty))
        attrs   (cond-> (token-data m)
                  title  (assoc :title title)
                  anchor (assoc :class (anchor-class anchor)))]
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
                         :keydown [:move-cursor k :event/key]
                         :click   inspect})
       (:word m)])))

(defn tokens
  "The token maps `ms` of `hit` from `source` (its :corpus and :structs)
  under the concordance `opts`, numbered from `offset` within the hit, as
  elements separated by spaces."
  [opts hit source offset ms]
  (interpose " " (map-indexed (fn [i m] (token opts hit source (+ offset i) m))
                              ms)))

(defn source-label
  "The hit's source as hiccup: its text title as a `<cite>` (a corpus text
  is a cited work) when present, else its most identifying structural value."
  [structs]
  (if-let [title (:text_title structs)]
    [:cite title]
    (or (:text_id structs) (first (vals structs)))))

(defn source-title
  "Tooltip text listing every structural annotation of a hit."
  [structs]
  (->> structs
       (map (fn [[k v]] (str (name k) ": " v)))
       (str/join "\n")))

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

(defn source-cell
  "The source of `hit` (see `source-label`) as the last cell of its row,
  linking to the reading page of its text, with the hit marked, where
  the hit knows its corpus; the label alone otherwise."
  [{:keys [corpus structs cpos anchors] :as hit}]
  [:td.kwic-structs {:title (source-title structs)}
   (when-let [label (source-label structs)]
     (if corpus
       [:a {:href (url/text corpus cpos (:matchend anchors))} label]
       label))])

(defn expand-control
  "The corpus position of `hit` as the control revealing its wider
  context, in `ui`, `expanded?` giving its state; the bare position where
  no `client?` answers the click."
  [ui client? hit expanded?]
  (let [cpos  (str (:cpos hit))
        ;; the accessible name opens with the visible position, so what is
        ;; said matches what is seen
        label (str cpos " · " (i18n/tr ui "Show or hide more context"))]
    (if-not client?
      cpos
      [:button (cond-> {:type          "button"
                        :aria-label    label
                        :aria-expanded (str (boolean expanded?))
                        :on            {:click [:toggle-context
                                                {:corpus   (:corpus hit)
                                                 :cpos     (:cpos hit)
                                                 :matchend (:matchend
                                                            (:anchors hit))}]}}
                 expanded? (assoc :aria-controls (context-id hit)))
       cpos])))

(defn hit-row
  "One KWIC `hit` as a table row under the concordance `opts`, with its
  corpus positions, its anchored tokens marked (see `anchored-tokens`)
  and `expanded?` its disclosure state."
  [{:keys [ui client?] :as opts} hit expanded?]
  (let [source (hit-source hit)
        opts   (assoc opts :anchored (anchored-tokens hit))
        {:keys [left match right structs anchors cpos]} hit
        nl     (count left)]
    [:tr.kwic-hit (position-data cpos anchors)
     ;; the position heads the row, so every other cell resolves a row
     ;; header as well as a column one
     [:th.kwic-cpos {:scope "row"} (expand-control ui client? hit expanded?)]
     [:td.kwic-left (tokens opts hit source 0 left)]
     [:td.kwic-match [:mark (tokens opts hit source nl match)]]
     [:td.kwic-right (tokens opts hit source (+ nl (count match)) right)]
     ;; last: between the position and the left context it stood in the
     ;; middle of the line a reader is there to read
     (source-cell hit)]))

(defn expanded-row
  "A full-width row under the concordance `opts` showing hit `ex`
  (fetched with wider context, so without metadata of its own) as
  flowing text, the match marked; its tokens are inspected with the
  source of `hit`, the row it expands, whose disclosure names it."
  [opts hit ex]
  (let [source (hit-source hit)
        ;; numbered past the row it expands: the two rows share a hit, so
        ;; numbering both from zero would give four elements one id and
        ;; four of them the cursor's tabindex
        base   (token-count hit)
        nl     (count (:left ex))
        nm     (count (:match ex))]
    [:tr.kwic-expanded {:id (context-id hit)}
     [:td {:colspan column-count}
      (tokens opts hit source base (:left ex)) " "
      [:mark (tokens opts hit source (+ base nl) (:match ex))] " "
      (tokens opts hit source (+ base nl nm) (:right ex))]]))

(defn status-row
  "A full-width row reporting `text` about an expansion in flight or
  failed, under `role` (\"status\" while loading, \"alert\" on failure)."
  [role text]
  [:tr.kwic-expanded
   ;; the only rows that appear without a page load, so the only ones a
   ;; live region is any use for
   [:td {:colspan column-count} [:span {:role role} text]]])

(defn hit-rows
  "The row(s) for `hit` under the concordance `opts` (see `concordance`),
  in its `:ui`: the KWIC row, followed by its expanded-context row when
  `:expanded` holds a fetched hit under its `hit-key`, an alert row when
  the fetch `failed`, or a status row while one is pending (`loading`,
  or anything else that is not a hit)."
  [{:keys [ui expanded] :as opts} hit]
  (let [ex  (get expanded (hit-key hit))
        row (hit-row opts hit (some? ex))]
    ;; always two children, the second sometimes nothing: a hit that
    ;; changes length shifts every row after it, and Replicant asks for an
    ;; explicit nil rather than a shorter list
    [row (cond
           (nil? ex)     nil
           (map? ex)     (expanded-row opts hit ex)
           (= failed ex) (status-row
                          "alert"
                          (i18n/tr ui "The context did not load."))
           :else         (status-row "status" (i18n/tr ui "Loading …")))]))

(defn corpus-group
  "The rows of `hits`, all from one corpus, as a row group under the
  concordance `opts` (see `concordance`): a header row naming the corpus
  and, from the per-corpus `:counts` of the search, how many hits it
  holds in all, then the hit rows with their expansions. A corpus whose
  query failed has no count.

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
         [:a {:href (url/corpus corpus)}
          [:code corpus]]
         (when size
           (list " " (widgets/count-badge (i18n/group-digits ui size))))]])
     (mapcat #(hit-rows opts %) hits)]))

(defn column-headers
  "The concordance's column headings in `ui`."
  [ui]
  ;; each heading carries its column's class, so a rule about a column
  ;; reaches the heading too rather than counting columns
  [:thead
   [:tr
    [:th.kwic-cpos {:scope "col"} (widgets/term ui :cpos false)]
    [:th.kwic-left {:scope "col"} (i18n/tr ui "left context")]
    [:th.kwic-match {:scope "col"} (widgets/term ui :match false)]
    [:th.kwic-right {:scope "col"} (i18n/tr ui "right context")]
    [:th.kwic-structs {:scope "col"} (i18n/tr ui "source")]]])

(def caption-id
  "The id of the concordance's caption, which names its scroll region."
  "concordance-caption")

(def region-id
  "The id of the region the concordance scrolls in. The client focuses it
  by this name rather than by the class the stylesheet uses, so renaming
  a style hook cannot break focus."
  "concordance")

(defn concordance
  "The KWIC `hits` of one result page as a table, one row group per corpus
  in the order the hits arrive, inside the region that scrolls it.

  `opts` carries the `:caption` naming the table, the `:ui` of its
  headings and controls, `:langs` (corpus to the language of its text),
  the per-corpus `:counts` heading each row group, `:expanded` (hit-key
  to a wider-context hit below its row), `:client?` where the script
  answering a token click runs, and `:cursor`, the one tabbable token."
  [hits {:keys [caption ui] :as opts}]
  (let [{:keys [expanded cursor]} opts
        ;; a cursor left behind by a hit that has since collapsed names no
        ;; token, which would leave the concordance with no tab stop at all
        in-range? (when-let [n (cursor-range hits expanded (first cursor))]
                    (< (second cursor) n))
        opts      (cond-> opts
                    (not in-range?) (assoc :cursor (default-cursor hits)))]
    ;; a KWIC line must not wrap, or its columns stop lining up, so the
    ;; table scrolls sideways in its own region; focusable because a
    ;; keyboard must be able to scroll it, and named because a focusable
    ;; region needs a name
    [:div.scroll {:id              region-id
                  :role            "region"
                  :tabindex        "0"
                  :aria-labelledby caption-id
                  ;; the panel describes what the cursor is on, so it has
                  ;; nothing to describe once the cursor is left behind
                  :on              {:focusout [:leave-concordance]}}
     [:table.kwic
      (when caption [:caption {:id caption-id} caption])
      (column-headers ui)
      (map #(corpus-group opts %) (partition-by :corpus hits))]]))

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
  `sample-sizes` with `sample` chosen, or the whole result when it names
  none. A size the list does not hold is offered beside them, so a URL
  naming one shows as the sample it is. It names the form it submits
  with, as `sort-control` does."
  [ui sample]
  (let [sizes (sort (cond-> (set sample-sizes) sample (conj sample)))]
    (widgets/select url/form-id "sample" (i18n/tr ui "Sample")
                    (list
                     (widgets/option (or sample "") "" (i18n/tr ui "all hits"))
                     (for [n sizes]
                       (widgets/option sample n (i18n/group-digits ui n)))))))

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
  `context-widths` with `context` (a number of words or a unit keyword)
  chosen, named by `context-label`. A number of words the list does not
  hold is offered among the numbers, in order. It names the form it
  submits with, as `sort-control` does."
  [ui context]
  (let [widths (if (or (keyword? context) (some #{context} context-widths))
                 context-widths
                 (into (vec (sort (conj (filterv number? context-widths)
                                        context)))
                       (filter keyword? context-widths)))
        ;; a unit is named in the URL, a number of words is the number
        value  (fn [width] (if (keyword? width) (name width) width))]
    (widgets/select url/form-id "context" (i18n/tr ui "Context")
                    (for [width widths]
                      (widgets/option (value context) (value width)
                                      (context-label ui width))))))

(defn concordance-section
  "The concordance view of the search in `state`: its sort, context and
  sample controls, the pagination above and below the table, the
  concordance itself and the download links, worded in the state's `:ui`
  and wrapped in dk.cst.corpus-probe.views.result/results-region.

  The result answers the params the search was `:asked` with, not the
  form's `:params`, which the client's form leaves behind at a change of
  mode."
  [{:keys [ui sort-modes asked result error langs expanded client?
           export-hrefs export-limit prev-href next-href]
    :as state}]
  (let [{:keys [counts hits size]} result
        position (when result (result/page-phrase ui result))]
    (result/results-region
     state
     (result/result-heading ui asked result error)
     (result/qualifiers ui asked result)
     (when (result/searched? result)
       ;; a search that found nothing has nothing to page, download or
       ;; count: the table would be a header over no rows and the exports
       ;; header-only files. A result emptied by the word its hits had
       ;; to be near keeps that one control, or the reader could not take
       ;; the word away again
       (if (zero? size)
         (list
          (when (:near result)
            (result/view-controls ui client? nil
                                  (result/near-control ui (:near result))
                                  true))
          [:p (i18n/tr ui "No hits.")])
         (list
          (result/view-controls ui client?
                                (list (sort-control ui sort-modes (:sort asked))
                                      " "
                                      (context-control ui (:context result))
                                      " "
                                      (sample-control ui (:sample result)))
                                (result/near-control ui (:near result))
                                (:near result))
          (result/pagination ui prev-href next-href position)
          (concordance hits {:caption  (widgets/term ui :kwic false)
                             :ui       ui
                             :langs    langs
                             :counts   counts
                             :expanded expanded
                             :client?  client?
                             :cursor   (:cursor state)})
          (result/pager-links ui prev-href next-href position)
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
