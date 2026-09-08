(ns dk.cst.corpus-probe.search.batch
  "The batches a search runs: [section command] pairs, each section naming
  what its command's output holds (see `batch-sections`); the hardened
  display profile every KWIC batch sets; and the paging arithmetic over
  the rows a batch reads."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.parse :as parse]))

(def hardened-profile
  "Display settings making KWIC output unambiguously parseable: every
  separator a TAB-framed marker letter (see
  dk.cst.corpus-probe.cwb.parse/markers), TAB never occurring inside a
  positional-attribute value."
  (let [{:keys [attribute token left right structure]} parse/markers]
    (str "set AttributeSeparator \"\t" attribute "\t\"; "
         "set TokenSeparator \"\t" token "\t\"; "
         "set LeftKWICDelim \"\t" left "\t\"; "
         "set RightKWICDelim \"\t" right "\t\"; "
         "set StructureDelimiter \"\t" structure "\t\"; "
         ;; inline annotation values are unescaped, may hold a TAB and can
         ;; crash CQP
         "set ShowTagAttributes off;")))

(def page-defaults
  "Default concordance paging: the first page of 25 hits."
  {:page 0 :page-size 25})

(def max-row
  "The highest row CQP can address: its range bounds are C ints, and a
  larger number wraps negative, which CQP reads as the entire result."
  Integer/MAX_VALUE)

(defn page-rows
  "The row range [from to] of page `page` with `page-size` hits per page:
  0-based and inclusive, as `cat` and `dump` take it; negative or zero
  values are clamped to the first page of one row, and the page to the
  last one below `max-row`."
  [page page-size]
  (let [size (max 1 page-size)
        from (* (min (max 0 page) (quot (- max-row size) size)) size)]
    [from (dec (+ from size))]))

(def kwic-defaults
  "Default KWIC display options, shared by `kwic-batch` and
  dk.cst.corpus-probe.search so the two cannot disagree."
  {:context 5
   :rows    (page-rows (:page page-defaults) (:page-size page-defaults))})

(def context-overshoot
  "How many words either side a concordance page fetches however few it
  shows, so that the line fills a wide screen and there is text past the
  width asked for to fade out and travel into."
  ;; the dial between what a page weighs and how far a reader gets before
  ;; one is fetched for them: every hit on the page carries this many
  ;; words twice over, whether or not they are ever looked at.
  ;; TODO: chosen against the toy corpora, where a text is shorter than
  ;; this and the clip decides instead. Weigh it again on the KU data,
  ;; where a text runs to hundreds of words and this is what a page costs
  30)

(defn fetch-context
  "The context a concordance page fetches to show `context`: never under
  `context-overshoot` words either side, nor under `reach`, which a
  reader who has travelled to the end of a line asks for. A unit of text
  is fetched as it is, being a bound of its own."
  [context reach]
  (if (number? context)
    (max context context-overshoot (or reach 0))
    context))

(defn context-spec
  "The width of context as CQP's Context option takes it: `context` as a
  number of words either side of the match, or as an s-attribute
  keyword, one region of which is shown either side.

  (context-spec 5)
  ;; => 5 words

  (context-spec :s)
  ;; => 1 s"
  [context]
  (if (keyword? context)
    (str "1 " (name context))
    (str (long context) " words")))

(defn setup-command
  "The command configuring one KWIC batch: the hardened display profile,
  `context` (see `context-spec`) either side of the match and, given
  `cache-dir`, the directory CQP reads and writes saved query results in."
  [context cache-dir]
  ;; DataDirectory before the activation: setting it rescans the corpus
  ;; list, resetting the active corpus
  (str (when cache-dir
         (str "set DataDirectory \""
              (command/valid-data-directory cache-dir) "\"; "))
       hardened-profile " set Context " (context-spec context) ";"))

(defn page-commands
  "The [section command] pairs displaying the rows `[from to]` of the
  query result named `nqr`: a :cat section showing the positional
  attributes `p-attrs` and tagging the regions of the s-attributes
  `shown` inline, the :dump anchors of the same rows and one :tabulate
  section per entry of `struct-attrs`."
  [nqr [from to] p-attrs struct-attrs shown]
  (let [span (str nqr " " from " " to)
        show (when-let [attrs (seq (concat (rest p-attrs) shown))]
               (str "show " (str/join " " (map #(str "+" (name %)) attrs))
                    "; "))]
    (into [[:cat (str show "cat " span ";")]
           [:dump (str "dump " span ";")]]
          ;; one tabulate per attribute, so that a whole line is one
          ;; value: an annotation value may hold a TAB
          (map (fn [attr]
                 [:tabulate (str "tabulate " span " match " (name attr) ";")]))
          struct-attrs)))

(defn size-batch
  "The batch counting the matches of `query` (raw CQP) in `corpus`:
  [section command] pairs, the activation, the query within the `filter`
  of `opts`, its narrowings, its `sample` and the size, in that order, so
  that the size reported is that of the hits kept. What every batch over
  the result of a query opens with (see `result-batch`)."
  [corpus query {:keys [filter sample] :as opts}]
  (let [sampling (command/sample-command sample)]
    (-> [[:corpus (str corpus ";")]
         [:query  (command/restricted-query query filter)]]
        (into (command/narrowing opts))
        (cond-> sampling (conj [:sample sampling]))
        (conj [:size "size Last;"]))))

(defn result-batch
  "The batch producing the result Last of `query` (raw CQP) in `corpus`:
  [section command] pairs, the setup with the `context` and `cache-dir`
  of `opts` (see `setup-command`), then its `size-batch`, the `sort` and,
  given `nqr`, the save. What reads the result follows (see `kwic-batch`
  and `export-batch`)."
  [corpus query {:keys [context cache-dir nqr sort]
                 :or   {context (:context kwic-defaults)}
                 :as   opts}]
  (-> [[:setup (setup-command context cache-dir)]]
      (into (size-batch corpus query opts))
      (conj [:sort (command/sort-command sort)])
      ;; saved after the sort: the order travels with the result into the
      ;; file
      (cond-> nqr (conj (let [nqr (command/valid-result-name nqr)]
                          [:save (str nqr " = Last; save " nqr ";")])))))

(defn kwic-batch
  "The batch running `query` (raw CQP) against `corpus` and returning the
  rows `:rows` of its result: the `result-batch` of `opts` followed by
  the `page-commands` of the rows, `p-attrs` and `struct-attrs` as they
  take them, tagging the regions of `text-attr` so a context wider than
  the hit's own text can be cut back to it."
  [corpus query {:keys [p-attrs struct-attrs rows text-attr]
                 :or   {rows (:rows kwic-defaults)}
                 :as   opts}]
  (into (result-batch corpus query opts)
        (page-commands "Last" rows p-attrs struct-attrs
                       (when text-attr [text-attr]))))

(defn tabulate-commands
  "The [section command] pairs printing the rows `[from to]` of the
  query result named `nqr` for an export, one line per hit: a :tabulate
  of the columns no TAB can occur in (the match's positions, `context`
  words either side of it, its words and its values of each of the
  `p-attrs` but word), then one :tabulate per entry of `struct-attrs`
  (see `page-commands`)."
  [nqr [from to] context p-attrs struct-attrs]
  (let [span (str nqr " " from " " to)]
    (into [[:tabulate
            (str "tabulate " span " match, matchend, "
                 "match[-" (long context) "]..match[-1] word, "
                 "match..matchend word, "
                 "matchend[1]..matchend[" (long context) "] word"
                 (str/join (map #(str ", match..matchend " (name %))
                                (remove #{:word} p-attrs)))
                 ";")]]
          (map (fn [attr]
                 [:tabulate (str "tabulate " span " match " (name attr) ";")]))
          struct-attrs)))

(defn export-batch
  "The batch running `query` (raw CQP) against `corpus` and printing the
  first `limit` rows of its result for an export: the `result-batch` of
  `opts` followed by the `tabulate-commands` of the rows, `context` a
  number of words and `p-attrs` and `struct-attrs` as they take them."
  [corpus query {:keys [context p-attrs struct-attrs limit] :as opts}]
  (into (result-batch corpus query opts)
        (tabulate-commands "Last" [0 (dec limit)] context p-attrs
                           struct-attrs)))

(defn text-batch
  "The batch reading one whole region of `corpus` as a KWIC row with no
  context: [section command] pairs as `kwic-batch` returns them, for
  `query` (raw CQP, whose one match is the region), showing `p-attrs`,
  tagging the regions of the s-attributes `shown` inline and fetching the
  `struct-attrs` as `page-commands` does."
  [corpus query {:keys [p-attrs struct-attrs shown]}]
  (into [[:setup  (setup-command 0 nil)]
         [:corpus (str corpus ";")]
         [:query  (command/locked-query query)]]
        (page-commands "Last" [0 0] p-attrs struct-attrs shown)))

(defn stored-kwic-batch
  "The batch returning the rows `:rows` of the saved query result named
  `nqr` of `corpus`: [section command] pairs as `kwic-batch` returns, no
  query run and nothing sorted, the matches and their order both coming
  from the save file."
  [corpus nqr {:keys [p-attrs struct-attrs context rows cache-dir text-attr]
               :or   {context (:context kwic-defaults)
                      rows    (:rows kwic-defaults)}}]
  (into [[:setup  (setup-command context cache-dir)]
         [:corpus (str corpus ";")]
         [:size   (str "size " (command/valid-result-name nqr) ";")]]
        (page-commands nqr rows p-attrs struct-attrs
                       (when text-attr [text-attr]))))

(defn stored-export-batch
  "The batch printing the first `limit` rows of the saved query result
  named `nqr` of `corpus` for an export, as `export-batch` prints a
  fresh one's; no query runs and nothing is sorted, as with
  `stored-kwic-batch`."
  [corpus nqr {:keys [context p-attrs struct-attrs cache-dir limit]}]
  (into [[:setup  (setup-command context cache-dir)]
         [:corpus (str corpus ";")]
         [:size   (str "size " (command/valid-result-name nqr) ";")]]
        (tabulate-commands nqr [0 (dec limit)] context p-attrs struct-attrs)))

(defn count-batch
  "The batch counting over the matches of `query` (raw CQP) in `corpus`
  with the `counting` commands: the `size-batch` of `opts` but for its
  sample, a count of a sample being no count, then one :count section per
  counting command, in their order."
  [corpus query opts counting]
  (into (size-batch corpus query (dissoc opts :sample))
        (map (fn [command] [:count command]))
        counting))

(defn stored-count-batch
  "The batch counting over the saved query result named `nqr` of `corpus`
  with the `counting` commands, as `count-batch` counts a fresh one: the
  result loaded as Last from the `cache-dir` of `opts`, its :size, its
  :sort and one :count section per counting command."
  [corpus nqr {:keys [cache-dir]} counting]
  (into [[:load (command/load-command corpus nqr cache-dir)]
         [:size "size Last;"]
         ;; back into corpus order, which a document frequency needs and
         ;; the save file did not keep
         [:sort "sort Last;"]]
        (map (fn [command] [:count command]))
        counting))

(defn batch-sections
  "Group the output `results` of `batch` (its [section command] pairs) by
  section: a map of section key to the vector of that section's output
  line vectors, in batch order. A key repeats (:tabulate per structural
  attribute, :count per counting command), so every key holds a vector."
  [batch results]
  (reduce (fn [m [[section _] lines]]
            (update m section (fnil conj []) lines))
          {}
          (map vector batch results)))
