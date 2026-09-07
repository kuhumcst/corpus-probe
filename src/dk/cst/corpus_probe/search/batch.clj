(ns dk.cst.corpus-probe.search.batch
  "The batches a search runs: [section command] pairs, each section naming
  what its command's output holds (see `batch-sections`), composed from
  the commands of dk.cst.corpus-probe.cwb.command; the hardened display
  profile every KWIC batch sets; and the paging arithmetic over the rows
  a batch reads."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.parse :as parse]))

(def hardened-profile
  "Display settings making KWIC output unambiguously parseable: every
  separator becomes a TAB-framed marker letter (see
  dk.cst.corpus-probe.cwb.parse/markers), and TAB can never occur inside
  a positional-attribute value. Inline annotation values are never shown
  (ShowTagAttributes off), since they are unescaped, may contain TAB, and
  can crash CQP (docs/research/gap-kwic-parsing.md)."
  (let [{:keys [attribute token left right structure]} parse/markers]
    (str "set AttributeSeparator \"\t" attribute "\t\"; "
         "set TokenSeparator \"\t" token "\t\"; "
         "set LeftKWICDelim \"\t" left "\t\"; "
         "set RightKWICDelim \"\t" right "\t\"; "
         "set StructureDelimiter \"\t" structure "\t\"; "
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
  0-based and inclusive, as `cat` and `dump` take it.

  Negative or zero values are clamped to the first page of one row, and
  the page to the last one below `max-row`, since CQP treats a negative
  range bound as the entire result."
  [page page-size]
  (let [size (max 1 page-size)
        from (* (min (max 0 page) (quot (- max-row size) size)) size)]
    [from (dec (+ from size))]))

(def kwic-defaults
  "Default KWIC display options, shared by `kwic-batch` and
  dk.cst.corpus-probe.search so the two cannot disagree: five tokens of
  context and the rows of the default page."
  {:context 5
   :rows    (page-rows (:page page-defaults) (:page-size page-defaults))})

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
  `context` (see `context-spec`) either side of the match and, when
  `cache-dir` is given, the directory CQP reads and writes saved query
  results in.

  DataDirectory is set here rather than beside the query because setting
  it rescans the corpus list, resetting the active corpus, so it has to
  come before the activation (docs/research/gap-nqr-persistence.md
  section 1)."
  [context cache-dir]
  (str (when cache-dir
         (str "set DataDirectory \""
              (command/valid-data-directory cache-dir) "\"; "))
       hardened-profile " set Context " (context-spec context) ";"))

(defn page-commands
  "The [section command] pairs displaying the rows `[from to]` of the
  query result named `nqr`: a `:cat` section, the `:dump` anchors of the
  same rows and one `:tabulate` section per entry of `struct-attrs`.

  `p-attrs` are the corpus's positional attributes (registry order) to
  show. Each structural attribute gets its own single-column `tabulate`
  command so that a whole output line is one annotation value: annotation
  values may legally contain TAB, so packing them into one TAB-separated
  row would misalign the columns. `cat` clamps the range to the result
  silently, so a range past the last row needs no check."
  [nqr [from to] p-attrs struct-attrs]
  (let [span (str nqr " " from " " to)
        show (when (next p-attrs)
               (str "show " (str/join " " (map #(str "+" (name %))
                                               (rest p-attrs))) "; "))]
    (into [[:cat (str show "cat " span ";")]
           [:dump (str "dump " span ";")]]
          (map (fn [attr]
                 [:tabulate (str "tabulate " span " match " (name attr) ";")]))
          struct-attrs)))

(defn size-batch
  "The batch counting the matches of `query` (raw CQP) in `corpus`:
  [section command] pairs, the activation, the query within the `filter`
  of `opts` (see dk.cst.corpus-probe.cwb.command/restricted-query), its
  narrowings (see dk.cst.corpus-probe.cwb.command/narrowing), its
  `sample` (see dk.cst.corpus-probe.cwb.command/sample-command) and the
  size: what every batch over the result of a query opens with (see
  `result-batch`).

  The narrowings and the sample come before the count, so that the size
  reported is that of the hits kept."
  [corpus query {:keys [filter sample] :as opts}]
  (let [sampling (command/sample-command sample)]
    (-> [[:corpus (str corpus ";")]
         [:query  (command/restricted-query query filter)]]
        (into (command/narrowing opts))
        (cond-> sampling (conj [:sample sampling]))
        (conj [:size "size Last;"]))))

(defn result-batch
  "The batch producing the result Last of `query` (raw CQP) in `corpus`:
  [section command] pairs, the setup with `context` and `cache-dir` (see
  `setup-command`), then the `size-batch` of `opts`, the `sort` (see
  dk.cst.corpus-probe.cwb.command/sort-command) and, given `nqr`, the
  save. What reads the result follows: a page of it (see `kwic-batch`)
  or every row (see `export-batch`).

  The result is named only after being sorted, since the sort order
  travels with it into the save file, which is what makes a stored
  result worth having."
  [corpus query {:keys [context cache-dir nqr sort]
                 :or   {context (:context kwic-defaults)}
                 :as   opts}]
  (-> [[:setup (setup-command context cache-dir)]]
      (into (size-batch corpus query opts))
      (conj [:sort (command/sort-command sort)])
      (cond-> nqr (conj (let [nqr (command/valid-result-name nqr)]
                          [:save (str nqr " = Last; save " nqr ";")])))))

(defn kwic-batch
  "The batch running `query` (raw CQP) against `corpus` and returning the
  rows `:rows` of its result: the `result-batch` of `opts` followed by
  the `page-commands` of the rows, [section command] pairs, each
  section naming what its command's output holds (see `batch-sections`).

  `rows` is the [from to] row range (see `page-rows`), `p-attrs` and
  `struct-attrs` are as `page-commands` takes them, and the rest of
  `opts` are `result-batch`'s. Given `nqr`, the result is also saved
  under that name for `stored-kwic-batch` to page later."
  [corpus query {:keys [p-attrs struct-attrs rows]
                 :or   {rows (:rows kwic-defaults)}
                 :as   opts}]
  (into (result-batch corpus query opts)
        (page-commands "Last" rows p-attrs struct-attrs)))

(defn tabulate-commands
  "The [section command] pairs printing the rows `[from to]` of the
  query result named `nqr` for an export, one line per hit: a :tabulate
  of the columns no TAB can occur in (the match's positions, `context`
  words either side of it, its words and its values of each of the
  `p-attrs` but word), then one :tabulate per entry of `struct-attrs`,
  since an annotation value may hold a TAB (see `page-commands`).
  `tabulate` clamps the range to the result as `cat` does, and prints a
  position outside the corpus as an empty word."
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
  `opts` followed by the `tabulate-commands` of the rows, with `context`
  a number of words either side, since `tabulate` takes token offsets
  only, and `p-attrs` and `struct-attrs` as they take them."
  [corpus query {:keys [context p-attrs struct-attrs limit] :as opts}]
  (into (result-batch corpus query opts)
        (tabulate-commands "Last" [0 (dec limit)] context p-attrs
                           struct-attrs)))

(defn text-batch
  "The batch reading one whole region of `corpus` as a KWIC row with no
  context: [section command] pairs as `kwic-batch` returns them, for
  `query` (raw CQP, whose one match is the region, see
  dk.cst.corpus-probe.cwb.command/position-query and CQP's `expand to`),
  showing the p-attributes `p-attrs`, tagging the regions of the
  s-attributes `shown` inline, where the row is split into the blocks it
  is read in, and fetching the `struct-attrs` as `page-commands` does."
  [corpus query {:keys [p-attrs struct-attrs shown]}]
  (-> [[:setup  (setup-command 0 nil)]
       [:corpus (str corpus ";")]
       [:query  (command/locked-query query)]]
      (cond-> (seq shown)
        (conj [:show (str "show "
                          (str/join " " (map #(str "+" (name %)) shown))
                          ";")]))
      (into (page-commands "Last" [0 0] p-attrs struct-attrs))))

(defn stored-kwic-batch
  "The batch returning the rows `:rows` of the saved query result named
  `nqr` of `corpus`: [section command] pairs, as `kwic-batch` returns.

  No query runs and nothing is sorted, the matches and their order both
  coming from the save file, so the options that decided them (the query,
  `sort`, `filter`, `subset`, `near` and `sample`) are none of this one's
  business."
  [corpus nqr {:keys [p-attrs struct-attrs context rows cache-dir]
               :or   {context (:context kwic-defaults)
                      rows    (:rows kwic-defaults)}}]
  (into [[:setup  (setup-command context cache-dir)]
         [:corpus (str corpus ";")]
         [:size   (str "size " (command/valid-result-name nqr) ";")]]
        (page-commands nqr rows p-attrs struct-attrs)))

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
  with the `counting` commands (see
  dk.cst.corpus-probe.cwb.command/count-command): the `size-batch` of
  `opts` but for its sample, a count of a sample being no count, then
  one :count section per counting command, in their order."
  [corpus query opts counting]
  (into (size-batch corpus query (dissoc opts :sample))
        (map (fn [command] [:count command]))
        counting))

(defn stored-count-batch
  "The batch counting over the saved query result named `nqr` of `corpus`
  with the `counting` commands, as `count-batch` counts a fresh one: the
  result loaded as Last from the `cache-dir` of `opts` (see
  dk.cst.corpus-probe.cwb.command/load-command), its :size, and the
  :sort putting it back into corpus order, which a document frequency
  needs and the save file did not keep, then one :count section per
  counting command."
  [corpus nqr {:keys [cache-dir]} counting]
  (into [[:load (command/load-command corpus nqr cache-dir)]
         [:size "size Last;"]
         [:sort "sort Last;"]]
        (map (fn [command] [:count command]))
        counting))

(defn batch-sections
  "Group the output `results` of `batch` (its [section command] pairs) by
  section: a map of section key to the vector of that section's output
  line vectors, in batch order.

  A section key repeats, `:tabulate` doing so once per structural
  attribute and `:count` once per counting command, so every key holds a
  vector of sections rather than one."
  [batch results]
  (reduce (fn [m [[section _] lines]]
            (update m section (fnil conj []) lines))
          {}
          (map vector batch results)))
