(ns dk.cst.corpus-probe.views.kwic
  "Hiccup for the KWIC concordance.

  The markup mirrors the structure CQP's own display modes imply (PLAN.md
  §7) and carries the corpus data as machine-readable HTML: the concordance
  is a table of hits, each row tagged with its corpus positions; every token
  is a span whose positional annotations are `data-*` attributes and whose
  surface form is the text content; the match is a `<mark>`.

  Tokens also carry a data-driven `:on` click handler dispatching `:inspect`.
  The server-side string renderer drops `:on`, so the same views render as
  plain HTML for first paint and become interactive once the client mounts."
  (:require [clojure.string :as str]))

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
  `data-*` attributes, and a click inspecting it with the `structs` of its
  hit."
  [structs m]
  [:span.token (merge {:title (token-title m)
                       :on    {:click [:inspect {:token m :structs structs}]}}
                      (token-data m))
   (:word m)])

(defn tokens
  "The token maps `ms` of a hit with structural metadata `structs`, as
  spans separated by spaces."
  [structs ms]
  (interpose " " (map #(token structs %) ms)))

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

(defn hit-row
  "One KWIC `hit` as a table row, the row carrying its corpus positions. The
  corpus-position cell is a disclosure button toggling the hit's wider
  context; `expanded?` sets its `aria-expanded` state."
  [{:keys [cpos left match right structs anchors]} expanded?]
  [:tr.hit (position-data cpos anchors)
   [:td.cpos [:button {:type          "button"
                       :title         "Toggle wider context"
                       :aria-expanded (str (boolean expanded?))
                       :on            {:click [:toggle-context
                                               {:cpos     cpos
                                                :matchend (:matchend anchors)}]}}
              (str cpos)]]
   [:th.structs {:scope "row" :title (source-title structs)}
    (source-label structs)]
   [:td.left (tokens structs left)]
   [:td.match [:mark (tokens structs match)]]
   [:td.right (tokens structs right)]])

(defn expanded-row
  "A full-width row showing hit `ex` (fetched with wider context) as flowing
  text, the match marked."
  [ex]
  [:tr.expanded
   [:td {:colspan 5}
    (tokens nil (:left ex)) " "
    [:mark (tokens nil (:match ex))] " "
    (tokens nil (:right ex))]])

(defn loading-row
  "A placeholder row shown beneath a hit while its wider context is loading."
  []
  [:tr.expanded [:td {:colspan 5} "…"]])

(defn hit-rows
  "The row(s) for `hit`: the KWIC row, followed by its expanded-context row
  when `expanded` holds a fetched hit for its corpus position, or a loading
  row while a placeholder is pending."
  [expanded hit]
  (let [ex  (get expanded (:cpos hit))
        row (hit-row hit (some? ex))]
    (cond
      (nil? ex) [row]
      (map? ex) [row (expanded-row ex)]
      :else     [row (loading-row)])))

(defn concordance
  "The KWIC `hits` of one result page as a table.

  `opts` may carry a `:caption` (hiccup or string describing the result), a
  `:lang` for the token content (the corpus text is in the corpus's own
  language while the surrounding UI is not) and `:expanded`, a map of corpus
  position to a wider-context hit to render beneath its row."
  [hits {:keys [caption lang expanded]}]
  [:table.kwic
   (when caption [:caption caption])
   [:tbody (cond-> {} lang (assoc :lang lang))
    (mapcat #(hit-rows expanded %) hits)]])
