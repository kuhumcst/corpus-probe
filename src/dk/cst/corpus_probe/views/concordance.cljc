(ns dk.cst.corpus-probe.views.concordance
  "Hiccup for the KWIC concordance: the table of hits, the controls that
  resubmit it, the section that is the concordance view of a result and
  the panel inspecting one of its tokens.

  The markup mirrors the structure CQP's own display modes imply (PLAN.md
  §7) and carries the corpus data as machine-readable HTML: the concordance
  is a table of hits, one row group per corpus, each row headed by its
  corpus position and tagged with its anchors; every token carries its
  positional annotations as `data-*` attributes and its surface form as the
  text content; the match is a `<mark>`.

  A token is a `<button>` only where the client runs, since without the
  script nothing answers a click, and a control that announces a role it
  cannot honour is worse than plain text. The server-side string renderer
  drops `:on`, so the same views render as static HTML for first paint and
  become interactive once the client mounts. The pure cursor arithmetic
  the client moves focus with lives here too."
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
  index to anchor name, for each anchor that is set and within the row.
  The match anchors are not among them, the match being marked as a
  whole."
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

  This is what lines one row up with another. A hit and its wider context
  are the same match with more of the text around it, so the match sits at
  offset zero in both, and moving between them lands on the same word
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
  :corpus and :structs of its hit.

  A `<button>` under `:client?`, so a keyboard can inspect a token and the
  browser announces it as the control it is; a plain span otherwise.

  The concordance's tokens are one cursor rather than hundreds of tab
  stops: only the token at `:cursor` is tabbable, and the arrow keys move
  the cursor between neighbours, as the APG asks of a grid of controls.
  Inspecting follows focus rather than waiting for a press, so moving the
  cursor moves what the panel describes.

  Under `:anchored` (see `anchored-tokens`), the token an anchor falls on
  carries the anchor's name as its class (see `anchor-class`) and in its
  title."
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
              :on       {:focus   inspect
                         :keydown [:move-cursor k]
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
  "The corpus position of `hit` as the control revealing its wider context,
  in `ui`, `expanded?` giving its state; the bare position where
  no client answers the click.

  Its accessible name opens with the visible position, so what is said
  matches what is seen, and while expanded it names the row it revealed."
  [ui client? hit expanded?]
  (let [cpos  (str (:cpos hit))
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
  "One KWIC `hit` as a table row under the concordance `opts`, the row
  carrying its corpus positions, its anchored tokens marked (see
  `anchored-tokens`) and `expanded?` its disclosure state.

  The corpus position is the row's header and its first cell, so every
  other cell resolves a row header as well as a column one. The source
  comes last: between the position and the left context it stood in the
  middle of the line a reader is there to read. It links to the whole
  text (see `source-cell`)."
  [{:keys [ui client?] :as opts} hit expanded?]
  (let [source (hit-source hit)
        opts   (assoc opts :anchored (anchored-tokens hit))
        {:keys [left match right structs anchors cpos]} hit
        nl     (count left)]
    [:tr.kwic-hit (position-data cpos anchors)
     [:th.kwic-cpos {:scope "row"} (expand-control ui client? hit expanded?)]
     [:td.kwic-left (tokens opts hit source 0 left)]
     [:td.kwic-match [:mark (tokens opts hit source nl match)]]
     [:td.kwic-right (tokens opts hit source (+ nl (count match)) right)]
     (source-cell hit)]))

(defn expanded-row
  "A full-width row showing hit `ex` (fetched with wider context, so
  without metadata of its own) as flowing text, the match marked; its
  tokens are inspected with the source of `hit`, the row it expands, whose
  disclosure names it."
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
  failed, under `role` (\"status\" while loading, \"alert\" on failure).

  These are the only rows that appear without a page load, so they are the
  only ones a live region is any use for."
  [role text]
  [:tr.kwic-expanded
   [:td {:colspan column-count} [:span {:role role} text]]])

(defn hit-rows
  "The row(s) for `hit` under the concordance `opts` (see `concordance`),
  in its `:ui`: the KWIC row, followed by its expanded-context row when
  `:expanded` holds a fetched hit under its `hit-key`, an alert row when
  the fetch `failed`, or a status row while one is pending (`loading`,
  or anything else that is not a hit); always two children, the second
  nil when there is no expansion, so a hit never changes how many rows
  it contributes."
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
  (linking to its info page in `:ui`) and, from the per-corpus `:counts`
  of the search, how many hits it holds in all, then the hit rows with
  their expansions. A corpus whose query failed has no count, its error
  being reported on its own. The group carries the corpus's own language
  from `:langs` when known, since the corpus text is in its own language
  while the surrounding UI is not.

  The count is beside the name it counts, so the reader is told how much
  of a corpus is under the rows they are reading where they are reading
  them. It is of the whole corpus rather than of the rows below it: a
  page holds as many hits as it holds, which is not a fact about any
  corpus."
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
  "The concordance's column headings in `ui`. The three token
  columns reuse the words the sort control already uses for them.

  Each heading carries its column's class, so that a rule about a column
  reaches the heading too rather than counting columns."
  [ui]
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
  by this name rather than by the class the stylesheet happens to use, so
  renaming a style hook cannot quietly break focus."
  "concordance")

(defn concordance
  "The KWIC `hits` of one result page as a table, one row group per corpus
  in the order the hits arrive, inside the region that scrolls it.

  A KWIC line must not wrap, or the columns that make it readable stop
  lining up, so the table scrolls sideways inside its own region rather
  than taking the whole document with it. The region is focusable because a
  keyboard must be able to scroll it, and named by the table's caption
  because a focusable region needs a name.

  `opts` may carry a `:caption` (hiccup or string naming the table),
  `:ui` (the lookup context of the headings and row controls), `:langs`
  (corpus name to the language of its own text), `:counts`, the
  per-corpus counts of the search, which head each row group (see
  `corpus-group`), `:expanded`, a map of `hit-key` to a wider-context
  hit to render beneath its row, `:client?`, true where the script that
  answers a token click is running, and `:cursor`, the [hit-key index]
  of the one tabbable token, which falls back to the first when it names
  no token the page still shows.

  Focus leaving the region closes the inspection panel, since the panel
  describes the token the cursor is on and there is nothing to describe
  once the reader has gone elsewhere."
  [hits {:keys [caption ui] :as opts}]
  (let [{:keys [expanded cursor]} opts
        ;; a cursor left behind by a hit that has since collapsed names no
        ;; token, which would leave the concordance with no tab stop at all
        in-range? (when-let [n (cursor-range hits expanded (first cursor))]
                    (< (second cursor) n))
        opts      (cond-> opts
                    (not in-range?) (assoc :cursor (default-cursor hits)))]
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
  naming a positional attribute (see
  dk.cst.corpus-probe.cwb.command/sort-attr) is the match by that
  attribute.

  Naming them here rather than in the commands namespace keeps the CQP
  command table free of anything the interface decides."
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
  "The sort control of the concordance in `ui`: a select over
  the `sort-modes` values (see dk.cst.corpus-probe.cwb.command/sort-modes)
  with `sort` chosen and each named by `sort-label`.

  It names the form it submits with (see
  dk.cst.corpus-probe.views.widgets/select), so it can sit beside the
  table it reorders rather than inside the query form: ordering a result
  is a different task from writing the query that produced it."
  [ui sort-modes sort]
  (widgets/select url/form-id "sort" (i18n/tr ui "Sort")
                  (for [value sort-modes]
                    (widgets/option sort value (sort-label ui value)))))

(def sample-sizes
  "The sample sizes the concordance offers, in display order: as many
  hits as a reader might work through by hand, a result larger than that
  being read by sampling it rather than by paging to the end.

  A hand-written URL may name any other size, which `sample-control`
  then shows beside these."
  [50 100 500 1000])

(defn sample-control
  "The sample control of the concordance in `ui`: a select over the
  `sample-sizes` with `sample` chosen, or the whole result when it names
  none.

  A size the list does not hold is offered beside them, so that a URL
  naming one shows as the sample it is rather than as the whole result.
  It names the form it submits with for the reason `sort-control` does."
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
  dk.cst.corpus-probe.cwb.corpus/units), one region of which is shown
  either side. The first is the usual width (see
  dk.cst.corpus-probe.search.batch/kwic-defaults). A hand-written URL may
  name any other number of words, which `context-control` then shows
  beside these."
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
  submits with, for the reason `sort-control` does."
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

(def near-distances
  "The distances the near control offers, in display order."
  [1 2 3 5 10])

(defn near-control
  "The proximity control of a result in `ui`: the word every hit must
  have nearby and how many words away it may be, from `near` (the :word
  and :distance in force, if any) and the `near-distances`.

  The word is typed rather than chosen, so it applies once the reader is
  done with it: a text field reports a change on Enter and on focus
  leaving it, and the change applies the view as a select's does. Enter
  alone could not be relied on: implicit submission does not reach a
  form from a field that only names it. The distance applies itself as
  the sort does. A distance the list does not hold is offered beside
  them, as a sample size is."
  [ui {:keys [word distance]}]
  (let [distance (or distance (parse-long (:distance url/defaults)))
        words    (fn [n] (str n " " (i18n/trn ui "word" "words" n)))]
    (list
     [:label {:for "near"} (i18n/tr ui "Near")]
     " "
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

(defn concordance-section
  "The concordance view of the search in `state`: when any corpus could be
  searched and found something, the sort, context and sample controls
  with the near control behind its disclosure (see
  dk.cst.corpus-probe.views.result/view-controls), the pagination above
  and below the table, the concordance with its `:expanded` hits, its
  `:langs` and the per-corpus counts that head its row groups, then the
  download links (`:export-hrefs`, exports holding at most
  `:export-limit` hits), all worded in the state's `:ui` and wrapped in
  the shared dk.cst.corpus-probe.views.result/results-region. The result
  answers the params the search was `:asked` with, not the form's
  `:params`, which the client's form leaves behind at a change of mode."
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
                                  (near-control ui (:near result))
                                  true))
          [:p (i18n/tr ui "No hits.")])
         (list
          (result/view-controls ui client?
                                (list (sort-control ui sort-modes (:sort asked))
                                      " "
                                      (context-control ui (:context result))
                                      " "
                                      (sample-control ui (:sample result)))
                                (near-control ui (:near result))
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
  "The id of the inspection panel, by which the client finds it (see
  dk.cst.corpus-probe.ui/leave-concordance!) rather than by the class the
  stylesheet uses, as it finds the region (see `region-id`)."
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
  while nothing is selected.

  Above the rail's breakpoint it takes the query column, so it sits beside
  the hits it describes without narrowing them; below it, it is a sheet at
  the foot of the viewport.

  It is not given focus when it opens: the cursor stays on the token so
  the arrow keys keep moving, and the panel describes whatever the
  cursor is on. That is why it is not a popover, which would put itself
  in the top layer, out of the grid, and want focus of its own. Escape
  closes it from the concordance. It can take focus, so that a click
  anywhere in it lands focus in the panel rather than on the page, and
  it reports focus leaving it, since the client closes it once focus
  has left both it and the concordance (see
  dk.cst.corpus-probe.ui/leave-concordance!).

  The group titles are in `ui`; the attribute names inside them are the
  corpus's own."
  [ui {:keys [token structs corpus cpos matchend] :as selected}]
  (when selected
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
