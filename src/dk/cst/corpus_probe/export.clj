(ns dk.cst.corpus-probe.export
  "Tabular text exports of concordances and frequency tables.

  Each export is first built as rows of strings, a header row first, with
  the columns the corresponding HTML table shows (the hit's positions,
  contexts, annotations; the value's counts per corpus), then rendered as
  TSV or CSV. TSV is the format CQP's own `tabulate` and `dump` print, but
  has no escaping, so the few characters that would break it are replaced;
  CSV is quoted per RFC 4180 and keeps every value intact."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.stats :as stats]))

(def hit-limit
  "The most hits a concordance export contains. A query over the large KU
  corpora can match millions of tokens; the page tells the user when the
  export is cut."
  10000)

(defn column-order
  "The keys of the maps `ms`, each once, in order of first appearance."
  [ms]
  (distinct (mapcat keys ms)))

(defn words
  "The surface forms of `tokens`, space-separated."
  [tokens]
  (str/join " " (map :word tokens)))

(defn annotations
  "The values of positional attribute `attr` of `tokens`, space-separated,
  as a `tabulate` range column prints them; empty when no token carries
  the attribute, as in a corpus without it."
  [attr tokens]
  (if (some #(contains? % attr) tokens)
    (str/join " " (map #(get % attr "") tokens))
    ""))

(defn kwic-table
  "The hits of concordance `result` as rows of strings: a header, then one
  row per hit with its corpus and positions, the contexts and the match as
  words, the match's other token annotations (one column each) and its
  structural annotations (one column each, over every attribute any hit
  carries)."
  [{:keys [hits] :as result}]
  (let [p-attrs (remove #{:word :open :close}
                        (column-order (mapcat :match hits)))
        s-attrs (column-order (map :structs hits))]
    (into [(-> ["corpus" "cpos" "matchend" "left" "match" "right"]
               (into (map #(str "match " (name %))) p-attrs)
               (into (map name) s-attrs))]
          (map (fn [{:keys [corpus cpos anchors left match right structs]}]
                 (-> [(str corpus) (str cpos) (str (:matchend anchors))
                      (words left) (words match) (words right)]
                     (into (map #(annotations % match)) p-attrs)
                     (into (map #(get structs % "")) s-attrs))))
          hits)))

(defn frequency-table
  "The merged frequency `result` as rows of strings: a header, then one row
  per value with, for every readable corpus, its frequency, its rate per
  million tokens and, when the result counts `:docs`, the texts it occurs
  in, plus the totals over several corpora, as the HTML table shows them
  but with every row."
  [{:keys [attr counts rows docs] :as result}]
  (let [readable (filter :tokens counts)
        total?   (boolean (next readable))
        tokens   (reduce + (map :tokens readable))
        cells    (fn [n t d]
                   (cond-> [(str n) (str (stats/per-million n t))]
                     docs (conj (str d))))
        heads    (fn [group]
                   (cond-> [(str group " frequency")
                            (str group " per million")]
                     docs (conj (str group " texts"))))]
    (into [(-> [(name attr)]
               (into (mapcat (comp heads :corpus)) readable)
               (cond-> total? (into (heads "total"))))]
          (map (fn [{:keys [value freqs total] doc-freqs :docs}]
                 (-> [value]
                     (into (mapcat (fn [{:keys [corpus tokens]}]
                                     (cells (get freqs corpus 0) tokens
                                            (get doc-freqs corpus 0))))
                           readable)
                     (cond-> total?
                       (into (cells total tokens
                                    (reduce + (vals doc-freqs))))))))
          rows)))

(defn tsv-value
  "Value `s` made safe for a TSV cell: the TAB and line breaks an
  annotation value may legally contain become spaces, since TSV cannot
  escape them."
  [s]
  (str/replace s #"[\t\r\n]" " "))

(defn tsv
  "Render `rows` of strings as TAB-separated text, one row per line."
  [rows]
  (apply str (map #(str (str/join "\t" (map tsv-value %)) "\n") rows)))

(defn csv-value
  "Value `s` as a CSV field: quoted, with inner quotes doubled, when it
  holds a quote, a comma or a line break (RFC 4180)."
  [s]
  (if (re-find #"[\",\r\n]" s)
    (str "\"" (str/replace s "\"" "\"\"") "\"")
    s))

(defn csv
  "Render `rows` of strings as RFC 4180 CSV text with CRLF line ends. The
  text starts with a byte order mark, so spreadsheet applications read it
  as UTF-8 rather than guessing a legacy encoding for the Danish letters."
  [rows]
  (str "\ufeff"
       (apply str (map #(str (str/join "," (map csv-value %)) "\r\n") rows))))

(def formats
  "The export formats by their `format` parameter value: how to render rows
  and the media type to serve."
  {"tsv" {:render tsv :content-type "text/tab-separated-values; charset=utf-8"}
   "csv" {:render csv :content-type "text/csv; charset=utf-8"}})
