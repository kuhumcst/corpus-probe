(ns dk.cst.corpus-probe.views.kwic
  "Hiccup for the KWIC concordance.

  The markup mirrors the structure CQP's own display modes imply (PLAN.md
  §7) and carries the corpus data as machine-readable HTML: the concordance
  is a table of hits, one row group per corpus, each row tagged with its
  corpus positions; every token is a span whose positional annotations are
  `data-*` attributes and whose surface form is the text content; the match
  is a `<mark>`.

  Tokens also carry a data-driven `:on` click handler dispatching `:inspect`.
  The server-side string renderer drops `:on`, so the same views render as
  plain HTML for first paint and become interactive once the client mounts."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]))

(defn hit-key
  "The key identifying `hit` in a concordance over several corpora: its
  corpus and its corpus position, since positions repeat across corpora."
  [{:keys [corpus cpos] :as hit}]
  [corpus cpos])

(defn token-title
  "Tooltip text for token map `m`: its non-word attributes joined by ' · '."
  [m]
  (->> (dissoc m :word :open :close)
       (vals)
       (remove str/blank?)
       (str/join " · ")))

(defn token-data
  "The annotations of token `m` as `data-*` attributes, one per positional
  attribute except the surface `:word` (the element's text) and the
  structure tags."
  [m]
  (into {} (for [[k v] (dissoc m :word :open :close)]
             [(keyword (str "data-" (name k))) v])))

(defn token
  "One token `m` as a span: its surface form as text, its annotations as
  `data-*` attributes, and a click inspecting it along with `source`, the
  :corpus and :structs of its hit."
  [source m]
  [:span.token (merge {:title (token-title m)
                       :on    {:click [:inspect (assoc source :token m)]}}
                      (token-data m))
   (:word m)])

(defn tokens
  "The token maps `ms` of a hit from `source` (its :corpus and :structs),
  as spans separated by spaces."
  [source ms]
  (interpose " " (map #(token source %) ms)))

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

(defn hit-row
  "One KWIC `hit` as a table row, the row carrying its corpus positions. The
  corpus-position cell is a disclosure button toggling the hit's wider
  context; `expanded?` sets its `aria-expanded` state."
  [{:keys [corpus cpos left match right structs anchors] :as hit} expanded?]
  (let [source (hit-source hit)]
    [:tr.hit (position-data cpos anchors)
     [:td.cpos [:button {:type          "button"
                         :title         "Toggle wider context"
                         :aria-expanded (str (boolean expanded?))
                         :on            {:click [:toggle-context
                                                 {:corpus   corpus
                                                  :cpos     cpos
                                                  :matchend (:matchend
                                                             anchors)}]}}
                (str cpos)]]
     [:th.structs {:scope "row" :title (source-title structs)}
      (source-label structs)]
     [:td.left (tokens source left)]
     [:td.match [:mark (tokens source match)]]
     [:td.right (tokens source right)]]))

(defn expanded-row
  "A full-width row showing hit `ex` (fetched with wider context, so
  without metadata of its own) as flowing text, the match marked; its
  tokens are inspected with `source`, the source of the hit it expands."
  [source ex]
  [:tr.expanded
   [:td {:colspan 5}
    (tokens source (:left ex)) " "
    [:mark (tokens source (:match ex))] " "
    (tokens source (:right ex))]])

(defn loading-row
  "A placeholder row shown beneath a hit while its wider context is loading."
  []
  [:tr.expanded [:td {:colspan 5} "…"]])

(defn hit-rows
  "The row(s) for `hit`: the KWIC row, followed by its expanded-context row
  when `expanded` holds a fetched hit under its `hit-key`, or a loading
  row while a placeholder is pending."
  [expanded hit]
  (let [ex  (get expanded (hit-key hit))
        row (hit-row hit (some? ex))]
    (cond
      (nil? ex) [row]
      (map? ex) [row (expanded-row (hit-source hit) ex)]
      :else     [row (loading-row)])))

(defn corpus-group
  "The rows of `hits`, all from one corpus, as a row group: a header row
  naming the corpus (linking to its info page), then the hit rows with
  their expansions from `expanded`. The group carries the corpus's
  language from `langs` when known, since the corpus text is in its own
  language while the surrounding UI is not."
  [expanded langs [{:keys [corpus]} :as hits]]
  (let [lang (get langs corpus)]
    [:tbody (cond-> {}
              corpus (assoc :data-corpus corpus)
              lang   (assoc :lang lang))
     (when corpus
       [:tr.corpus
        [:th {:scope "rowgroup" :colspan 5}
         [:a {:href (corpus-views/corpus-href corpus)} [:code corpus]]]])
     (mapcat #(hit-rows expanded %) hits)]))

(defn concordance
  "The KWIC `hits` of one result page as a table, one row group per corpus
  in the order the hits arrive.

  `opts` may carry a `:caption` (hiccup or string describing the result),
  `:langs` (corpus name to language code) and `:expanded`, a map of
  `hit-key` to a wider-context hit to render beneath its row."
  [hits {:keys [caption langs expanded]}]
  [:table.kwic
   (when caption [:caption caption])
   (map #(corpus-group expanded langs %) (partition-by :corpus hits))])
