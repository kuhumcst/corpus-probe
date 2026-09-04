(ns dk.cst.corpus-probe.views.kwic
  "Hiccup for the KWIC concordance.

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
  become interactive once the client mounts."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]))

(def column-count
  "How many columns a concordance row has, which a full-width row spans."
  5)

(def failed
  "Marks an expansion whose context could not be fetched, as `::loading`
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

(defn token-offset
  "How far token `i` of `hit` is from the start of its match: negative in
  the left context, zero at the first token of the match, positive after
  it.

  This is what lines one row up with another. A hit and its wider context
  are the same match with more of the text around it, so the match sits at
  offset zero in both, and moving between them lands on the same word
  rather than on the same column."
  [hit i]
  (- i (count (:left hit))))

(defn offset-token
  "The index of the token of `hit` at `offset` from its match, or the
  nearest one when the offset falls outside the row: a narrow row cannot
  answer an offset only a wide one has."
  [hit offset]
  (min (dec (token-count hit))
       (max 0 (+ offset (count (:left hit))))))

(defn cursor-range
  "How many tokens the cursor can visit at `hit-key`, given the `expanded`
  map: the hit's own tokens, plus its wider context's when one is showing."
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
  cursor moves what the panel describes."
  [{:keys [client? cursor] :as opts} hit source i m]
  (let [k       [(hit-key hit) i]
        inspect [:inspect (assoc source :token m)]
        title   (token-title m)
        attrs   (cond-> (token-data m)
                  title (assoc :title title))]
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
  "The source of `hit` that its tokens are inspected with: its :corpus and
  its structural metadata :structs."
  [hit]
  (select-keys hit [:corpus :structs]))

(defn context-control
  "The corpus position of `hit` as the control revealing its wider context,
  in language `lang`, `expanded?` giving its state; the bare position where
  no client answers the click.

  Its accessible name opens with the visible position, so what is said
  matches what is seen, and while expanded it names the row it revealed."
  [lang client? hit expanded?]
  (let [cpos (str (:cpos hit))]
    (if-not client?
      cpos
      [:button (cond-> {:type          "button"
                        :aria-label    (str cpos " · "
                                            (i18n/tr lang :wider-context))
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
  carrying its corpus positions and `expanded?` its disclosure state.

  The corpus position is the row's header and its first cell, so every
  other cell resolves a row header as well as a column one."
  [{:keys [lang client?] :as opts} hit expanded?]
  (let [source (hit-source hit)
        {:keys [left match right structs anchors cpos]} hit
        nl     (count left)]
    [:tr.hit (position-data cpos anchors)
     [:th.cpos {:scope "row"} (context-control lang client? hit expanded?)]
     [:td.structs {:title (source-title structs)} (source-label structs)]
     [:td.left (tokens opts hit source 0 left)]
     [:td.match [:mark (tokens opts hit source nl match)]]
     [:td.right (tokens opts hit source (+ nl (count match)) right)]]))

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
    [:tr.expanded {:id (context-id hit)}
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
  [:tr.expanded
   [:td {:colspan column-count} [:span {:role role} text]]])

(defn hit-rows
  "The row(s) for `hit` under the concordance `opts` (see `concordance`),
  in its UI language `:lang`: the KWIC row, followed by its
  expanded-context row when `:expanded` holds a fetched hit under its
  `hit-key`, a status row while one is pending, or an alert row when the
  fetch failed; always two children, the second nil when there is no
  expansion, so a hit never changes how many rows it contributes."
  [{:keys [lang expanded] :as opts} hit]
  (let [ex  (get expanded (hit-key hit))
        row (hit-row opts hit (some? ex))]
    ;; always two children, the second sometimes nothing: a hit that
    ;; changes length shifts every row after it, and Replicant asks for an
    ;; explicit nil rather than a shorter list
    [row (cond
           (nil? ex)     nil
           (map? ex)     (expanded-row opts hit ex)
           (= failed ex) (status-row "alert" (i18n/tr lang :context-failed))
           :else         (status-row "status" (i18n/tr lang :loading)))]))

(defn corpus-group
  "The rows of `hits`, all from one corpus, as a row group under the
  concordance `opts` (see `concordance`): a header row naming the corpus
  (linking to its info page in the UI language `:lang`), then the hit rows
  with their expansions. The group carries the corpus's own language from
  `:langs` when known, since the corpus text is in its own language while
  the surrounding UI is not."
  [{:keys [lang langs] :as opts} [{:keys [corpus]} :as hits]]
  (let [corpus-lang (get langs corpus)]
    [:tbody (cond-> {}
              corpus      (assoc :data-corpus corpus)
              corpus-lang (assoc :lang corpus-lang))
     (when corpus
       [:tr.corpus
        [:th {:scope "rowgroup" :colspan column-count}
         [:a {:href (corpus-views/corpus-href lang corpus)}
          [:code corpus]]]])
     (mapcat #(hit-rows opts %) hits)]))

(defn column-headers
  "The concordance's column headings in language `lang`. The three token
  columns reuse the words the sort control already uses for them."
  [lang]
  [:thead
   [:tr
    [:th {:scope "col"} (i18n/tr lang :position)]
    [:th {:scope "col"} (i18n/tr lang :source)]
    [:th {:scope "col"} (i18n/tr lang :sort-left)]
    [:th {:scope "col"} (i18n/tr lang :sort-word)]
    [:th {:scope "col"} (i18n/tr lang :sort-right)]]])

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
  `:lang` (the UI language of the headings and row controls), `:langs`
  (corpus name to the language of its own text), `:expanded`, a map of
  `hit-key` to a wider-context hit to render beneath its row, `:client?`,
  true where the script that answers a token click is running, and
  `:cursor`, the [hit-key index] of the one tabbable token, which falls
  back to the first when it names no token the page still shows.

  Focus leaving the region closes the inspection panel, since the panel
  describes the token the cursor is on and there is nothing to describe
  once the reader has gone elsewhere."
  [hits {:keys [caption lang] :as opts}]
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
      (column-headers lang)
      (map #(corpus-group opts %) (partition-by :corpus hits))]]))
