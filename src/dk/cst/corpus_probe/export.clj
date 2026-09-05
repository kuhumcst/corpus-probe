(ns dk.cst.corpus-probe.export
  "Tabular text exports of concordances and frequency tables.

  Each export is built as rows of strings, a header row first, with the
  columns the corresponding HTML table shows (the hit's positions,
  contexts, annotations; the value's counts per corpus), then rendered
  line by line as TSV or CSV: a frequency table whole, a concordance as
  its corpora answer. TSV is the format CQP's own `tabulate` and `dump`
  print, but has no escaping, so the few characters that would break it
  are replaced; CSV is quoted per RFC 4180 and keeps every value
  intact."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.stats :as stats]))

(def hit-limit
  "The most hits a concordance export holds. A query over the large KU
  corpora can match millions of tokens, and the page tells the reader
  when the export is cut. Half a million tabulated rows were measured
  at eight seconds and about 120 MB of heap through the driver, which
  is what one corpus's share of an export costs while it is written."
  500000)

(defn kwic-header
  "The header of a concordance export over corpora whose positional
  attributes but word are `p-attrs` and whose annotated s-attributes
  are `struct-attrs`: the hit's corpus and positions, its contexts and
  its match as words, the match's value of each positional attribute
  and its structural annotations."
  [p-attrs struct-attrs]
  (-> ["corpus" "cpos" "matchend" "left" "match" "right"]
      (into (map #(str "match " (name %))) p-attrs)
      (into (map name) struct-attrs)))

(defn kwic-rows
  "The rows of `export` (see dk.cst.corpus-probe.search/export!) under
  the columns of `kwic-header` for `p-attrs` and `struct-attrs`: the
  corpus first, then the positions, contexts and match, then the value
  of each column, empty for one the corpus lacks."
  [p-attrs struct-attrs {:keys [corpus annotations rows]}]
  (map (fn [[cpos matchend left match right & values]]
         (let [m (zipmap annotations values)]
           (-> [corpus cpos matchend left match right]
               (into (map #(get m % "")) p-attrs)
               (into (map #(get m % "")) struct-attrs))))
       rows))

(defn frequency-table
  "The merged frequency `result` as rows of strings: a header, then one row
  per value with, for every readable corpus, its frequency, its rate per
  million tokens, those tokens when the result is `:sized` (the text of
  the value rather than the corpus) and, when it counts `:docs`, the
  texts it occurs in, plus the totals over several corpora, as the HTML
  table shows them but with every row."
  [{:keys [attr counts rows docs sized] :as result}]
  (let [readable (filter :tokens counts)
        total?   (boolean (next readable))
        tokens   (reduce + (map :tokens readable))
        cells    (fn [n t d]
                   (cond-> [(str n) (str (stats/per-million n t))]
                     sized (conj (str t))
                     docs  (conj (str d))))
        heads    (fn [group]
                   (cond-> [(str group " frequency")
                            (str group " per million")]
                     sized (conj (str group " tokens"))
                     docs  (conj (str group " texts"))))]
    (into [(-> [(name attr)]
               (into (mapcat (comp heads :corpus)) readable)
               (cond-> total? (into (heads "total"))))]
          (map (fn [{:keys [value freqs total]
                     doc-freqs :docs row-tokens :tokens}]
                 (-> [value]
                     (into (mapcat (fn [{:keys [corpus tokens]}]
                                     (cells (get freqs corpus 0)
                                            (if sized
                                              (get row-tokens corpus 0)
                                              tokens)
                                            (get doc-freqs corpus 0))))
                           readable)
                     (cond-> total?
                       (into (cells total
                                    (if sized
                                      (reduce + (vals row-tokens))
                                      tokens)
                                    (reduce + (vals doc-freqs))))))))
          rows)))

(defn crosstab-table
  "The cross-tabulated frequency `result` (see
  dk.cst.corpus-probe.frequency/frequency-table! under :by) as rows of
  strings: a header naming the attribute, each of the `:columns` and the
  total, then, when the result is `:sized`, a row of the tokens each
  column measures against, then one row per value with its frequency in
  each column and, when sized, its rate per million of the column's
  tokens, as the HTML table shows them but with every row."
  [{:keys [attr counts columns rows sized] :as result}]
  (let [tokens (reduce + (map :tokens (filter :tokens counts)))
        cells  (fn [n t]
                 (cond-> [(str n)]
                   sized (conj (str (stats/per-million n t)))))
        heads  (fn [group]
                 (cond-> [(str group " frequency")]
                   sized (conj (str group " per million"))))
        blanks (fn [t] (cond-> [(str t)] sized (conj "")))]
    (-> [(-> [(name attr)]
             (into (mapcat (comp heads :value)) columns)
             (into (heads "total")))]
        (cond-> sized
          (conj (-> ["tokens"]
                    (into (mapcat (comp blanks :tokens)) columns)
                    (into (blanks tokens)))))
        (into (map (fn [{:keys [value total] freqs :cells}]
                     (-> [value]
                         (into (mapcat (fn [{:keys [tokens] col :value}]
                                         (cells (get freqs col 0) tokens)))
                               columns)
                         (into (cells total tokens)))))
              rows))))

(defn tsv-value
  "Value `s` made safe for a TSV cell: the TAB and line breaks an
  annotation value may legally contain become spaces, since TSV cannot
  escape them."
  [s]
  (str/replace s #"[\t\r\n]" " "))

(defn tsv-line
  "Render `row` of strings as one line of TAB-separated text."
  [row]
  (str (str/join "\t" (map tsv-value row)) "\n"))

(defn tsv
  "Render `rows` of strings as TAB-separated text, one row per line."
  [rows]
  (apply str (map tsv-line rows)))

(defn csv-value
  "Value `s` as a CSV field: quoted, with inner quotes doubled, when it
  holds a quote, a comma or a line break (RFC 4180)."
  [s]
  (if (re-find #"[\",\r\n]" s)
    (str "\"" (str/replace s "\"" "\"\"") "\"")
    s))

(defn csv-line
  "Render `row` of strings as one RFC 4180 CSV line, CRLF-terminated."
  [row]
  (str (str/join "," (map csv-value row)) "\r\n"))

(def bom
  "The byte order mark CSV text starts with, so spreadsheet applications
  read it as UTF-8 rather than guessing a legacy encoding for the Danish
  letters."
  "\ufeff")

(defn csv
  "Render `rows` of strings as RFC 4180 CSV text with CRLF line ends,
  opening with the `bom`."
  [rows]
  (str bom (apply str (map csv-line rows))))

(def formats
  "The export formats by their `format` parameter value: what the text
  opens with, how to render one row as a line and a whole table of
  rows, and the media type to serve."
  {"tsv" {:preamble     ""
          :line         tsv-line
          :render       tsv
          :content-type "text/tab-separated-values; charset=utf-8"}
   "csv" {:preamble     bom
          :line         csv-line
          :render       csv
          :content-type "text/csv; charset=utf-8"}})
