(ns dk.cst.corpus-probe.search
  "Running a search of this app: one function call in, plain data out. A
  KWIC page, a text, an export or a match count for one corpus, and a
  concordance over several. The helpers: the options as one corpus runs
  them (dk.cst.corpus-probe.search.opts, the trust boundary of the web
  layer), the batches of commands (dk.cst.corpus-probe.search.batch), a
  result read from the saved query result when one is stored and run
  afresh otherwise (dk.cst.corpus-probe.search.result), the saved results
  themselves (dk.cst.corpus-probe.search.cache), the breakdowns of the
  matches (dk.cst.corpus-probe.search.frequency) and the TSV and CSV
  exports (dk.cst.corpus-probe.search.export).

  Each round trip composes the batches, the child-process driver
  (dk.cst.corpus-probe.cwb) and its output parsers
  (dk.cst.corpus-probe.cwb.parse).

  Every search takes a :filter option, a metadata filter, and a :patterns
  option, the regexes its values may match instead (see
  dk.cst.corpus-probe.cwb.command/filter-query), restricting it to the
  matching regions of each corpus."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.parse :as parse]
            [dk.cst.corpus-probe.search.batch :as batch]
            [dk.cst.corpus-probe.search.cache :as cache]
            [dk.cst.corpus-probe.search.opts :as opts]
            [dk.cst.corpus-probe.search.result :as result]))

(defn size!
  "The number of matches of CQP `query` in `corpus` via `ctx`, within the
  :filter of `opts` when there is one, kept within its :within unit (see
  dk.cst.corpus-probe.search.opts/within-attr!), narrowed to its :subset
  and :near (see dk.cst.corpus-probe.cwb.command/narrowing) and reduced
  to its :sample, by running the query (see
  dk.cst.corpus-probe.search.batch/size-batch).

  Counted once and then remembered (see
  dk.cst.corpus-probe.search.cache/count!), because paging a search over
  several corpora counts the ones contributing no rows to the page again
  on every page, and that is a whole query each time. A narrowing of
  nothing is nothing, and is answered without CQP (see
  dk.cst.corpus-probe.search.result/narrowing-nothing?), but remembered
  like any other count, so that a page can tell it from a count still to
  be made (see `known-size`). Throws ex-info when CQP reports an error,
  times out or dies."
  ([ctx corpus query]
   (size! ctx corpus query {}))
  ([ctx corpus query opts]
   (let [[ctx* query* opts*] (opts/size-args! ctx corpus query opts)]
     (cache/count! ctx* corpus query* opts*
                   #(if (result/narrowing-nothing? ctx corpus query opts size!)
                      0
                      (result/match-count
                       (result/run-result! (cwb/running-ctx ctx*) corpus query*
                                           (batch/size-batch corpus query*
                                                             opts*))))))))

(defn remember-size!
  "Remember `n` as the number of matches of CQP `query` in `corpus` via
  `ctx` under `opts` (as for `size!`).

  A KWIC batch counts the matches it pages through, and the count of the
  whole search asks for that corpus once more, so a page hands its
  count over rather than leaving `size!` to run the query again."
  [ctx corpus query opts n]
  (let [[ctx query opts] (opts/size-args! ctx corpus query opts)]
    (cache/count! ctx corpus query opts (constantly n))))

(defn known-size
  "The number of matches of CQP `query` in `corpus` via `ctx` under `opts`
  (as for `size!`) when it has been counted before, else nil (see
  dk.cst.corpus-probe.search.cache/known-count). Runs nothing."
  [ctx corpus query opts]
  (let [[ctx query opts] (opts/size-args! ctx corpus query opts)]
    (cache/known-count ctx corpus query opts)))

(defn corpus-size!
  "The size of `query`'s result in `corpus` via `ctx` (`opts` as for
  `size!`) without failing: {:corpus ... :size <n>}, or {:corpus ...
  :error <error map>} when the query cannot run there."
  [ctx corpus query opts]
  (cwb/attempt corpus
               (fn [] {:corpus corpus :size (size! ctx corpus query opts)})))

(defn corpus-sizes!
  "The sizes of `query`'s result in each of `corpora` via `ctx` (`opts`
  as for `size!`), queried in parallel (see
  dk.cst.corpus-probe.cwb/parallelism) until `deadline` passes, after
  which the rest are reported as timed out. Returns one `corpus-size!`
  map per corpus in the given order."
  [ctx corpora query deadline opts]
  (vec (cwb/pmap-n (cwb/parallelism ctx)
                   (fn [corpus]
                     (if (cwb/overdue? deadline)
                       {:corpus corpus :error {:type :timeout}}
                       (corpus-size! (cwb/within-deadline ctx deadline)
                                     corpus query opts)))
                   corpora)))

(defn known-sizes
  "The sizes of `query`'s result in those of `corpora` counted before
  (see `known-size`) via `ctx` (`opts` as for `size!`), running nothing:
  one {:corpus ... :size <n>} map per corpus that has one, in the given
  order. A corpus whose count cannot even be looked up, its filter naming
  an attribute it lacks, is left to the count that runs, which reports
  the error."
  [ctx corpora query opts]
  (into []
        (keep (fn [corpus]
                (when-let [n (try (known-size ctx corpus query opts)
                                  (catch Exception _ nil))]
                  {:corpus corpus :size n})))
        corpora))

(defn kwic-sections!
  "The sections of one KWIC page of `query` in `corpus` via `ctx` under
  `opts` (see dk.cst.corpus-probe.search.opts/kwic-opts!): read from the
  saved query result when one is stored (see
  dk.cst.corpus-probe.search.result/read-stored!), and run afresh
  otherwise (see dk.cst.corpus-probe.search.result/run-fresh!).

  Requests that want the same page of the same search share one run of it
  while it is in flight (see dk.cst.corpus-probe.search.cache/share!), so
  that a reader who reloads during a long sort waits for the sort already
  running rather than starting a second one. They want the same page when
  they would run the same batch, so the batch is the key: anything a
  future option changes about the output changes it too, without anyone
  having to remember to add it."
  [ctx corpus query {:keys [nqr] :as opts}]
  (let [fetch #(or (result/read-stored! ctx corpus query opts
                                        batch/stored-kwic-batch
                                        result/intact?)
                   (result/run-fresh! ctx corpus query opts
                                      batch/kwic-batch))]
    (if nqr
      (cache/share! (batch/stored-kwic-batch corpus nqr opts) fetch)
      (fetch))))

(defn kwic!
  "Run CQP `query` against `corpus` (an uppercase CQP corpus name) through
  the installation described by `ctx` (see dk.cst.corpus-probe.cwb) and
  return the hits in one row range as data.

  Returns {:corpus ... :query ... :size <total hits> :rows [from to]
  :hits [hit ...]} where each hit combines the parsed KWIC line (:cpos :left
  :match :right), its anchors from `dump` (:anchors) and its structural
  metadata (:structs). `opts` accepts :rows (the [from to] row range, see
  dk.cst.corpus-probe.search.batch/page-rows; default the first page),
  :context (a number of tokens, or a unit of text as
  dk.cst.corpus-probe.search.opts/corpus-context resolves it), :sort (a
  sort mode, or a positional attribute to sort by; see
  dk.cst.corpus-probe.search.opts/corpus-sort!), :filter (a metadata
  filter), :sample (how many of the matches to keep, drawn at random; see
  dk.cst.corpus-probe.cwb.command/sample-command), :near (a word the
  matches must have nearby, marked as their keyword; see
  dk.cst.corpus-probe.cwb.command/near-command), :within (a unit of text
  the matches are kept within, see
  dk.cst.corpus-probe.search.opts/within-attr!) and :struct-attrs
  (defaults to every annotated s-attribute of the corpus; anything not in
  that inventory is rejected).

  When `ctx` keeps a cache (see dk.cst.corpus-probe.search.cache), the
  result is saved there and a later page of the same query, filter and
  sort mode is read back from it rather than queried and sorted again; a
  false :cache? in `opts` leaves it out of the cache, for a query nothing
  will ask for twice. A narrowing of nothing is answered without CQP (see
  dk.cst.corpus-probe.search.result/narrowing-nothing?).

  Throws ex-info when CQP reports an error, times out or dies."
  ([ctx corpus query]
   (kwic! ctx corpus query {}))
  ([ctx corpus query opts]
   (let [ctx   (corpus/corpus-ctx ctx corpus)
         query (opts/corpus-query! ctx corpus query (:within opts))]
     (if (result/narrowing-nothing? ctx corpus query (dissoc opts :within)
                                    size!)
       {:corpus corpus
        :query  query
        :size   0
        :rows   (:rows opts (:rows batch/kwic-defaults))
        :hits   []}
       (let [opts     (opts/kwic-opts! ctx corpus query opts)
             {:keys [p-attrs struct-attrs rows]} opts
             sections (kwic-sections! ctx corpus query opts)
             {[cat-lines]  :cat
              [dump-lines] :dump
              tab-sections :tabulate} sections
             hits     (parse/kwic->hits p-attrs cat-lines)
             anchors  (parse/dump->anchors dump-lines)
             structs  (when (seq struct-attrs)
                        (mapv #(zipmap struct-attrs %)
                              (parse/tabulate->rows tab-sections)))]
         (when (not= (count hits) (count anchors))
           ;; cat and dump disagree only when CQP printed something other
           ;; than the requested rows, so the page cannot be trusted
           (throw (ex-info "KWIC output misaligned"
                           {:corpus corpus
                            :error  {:type     :misaligned
                                     :expected (count hits)
                                     :received (count anchors)}})))
         {:corpus corpus
          :query  query
          :size   (result/match-count sections)
          :rows   rows
          :hits   (mapv (fn [hit anchor struct]
                          (cond-> (assoc hit :anchors anchor)
                            struct (assoc :structs struct)))
                        hits anchors (or structs (repeat nil)))})))))

(defn blocks
  "The `tokens` of a text in the blocks it is read in: a new block
  wherever a region of `unit` (an s-attribute keyword) opens, by the
  :open tags the tokens carry; one block when `unit` is nil."
  [unit tokens]
  (if unit
    (reduce (fn [blocks token]
              (if (or (empty? blocks) (some #{unit} (:open token)))
                (conj blocks [token])
                (update blocks (dec (count blocks)) conj token)))
            []
            tokens)
    [tokens]))

(defn text!
  "The text of `corpus` (an uppercase CQP corpus name) holding corpus
  position `cpos` via `ctx`, for reading: {:corpus ... :from <n> :to <n>
  :structs {...} :blocks [[word ...] ...]}, the positions of its first
  and last token, its structural annotations and its words in the
  blocks it is read in (see `blocks`): the paragraphs where the corpus
  marks them, its sentences otherwise, one block where it marks
  neither. Nil when no text holds the position.

  A text is a region of the corpus's own text attribute (see
  dk.cst.corpus-probe.cwb.corpus/unit-attrs), read as one match with no
  context (see dk.cst.corpus-probe.search.batch/text-batch); a corpus
  marking no texts has none to read, and says so with a :no-texts error.
  Throws ex-info when CQP reports an error, times out or dies."
  [ctx corpus cpos]
  (let [ctx        (corpus/corpus-ctx ctx corpus)
        attributes (corpus/attributes! ctx corpus)
        text       (or (corpus/unit-attr attributes :text)
                       (throw (ex-info "This corpus marks no texts"
                                       {:corpus corpus
                                        :error  {:type :no-texts}})))
        unit       (or (corpus/unit-attr attributes :paragraph)
                       (corpus/unit-attr attributes :sentence))
        query      (str (command/position-query cpos cpos) " expand to "
                        (name text))
        annotated  (corpus/attr-names corpus/annotated-s-attr? attributes)
        batch      (batch/text-batch corpus query
                                     {:p-attrs      [:word]
                                      :struct-attrs annotated
                                      :shown        (some-> unit vector)})
        {[cat-lines] :cat [dump-lines] :dump tab-sections :tabulate}
        (result/run-result! (cwb/running-ctx ctx) corpus query batch)]
    (when-let [hit (first (parse/kwic->hits [:word] cat-lines))]
      (let [{:keys [match matchend]} (first (parse/dump->anchors dump-lines))]
        {:corpus  corpus
         :from    match
         :to      matchend
         :structs (zipmap annotated (first (parse/tabulate->rows tab-sections)))
         :blocks  (mapv #(mapv :word %) (blocks unit (:match hit)))}))))

(def export-unit-width
  "How many words an export prints either side of a hit where the
  concordance shows a unit of text: `tabulate` takes token offsets
  only, and twenty covers most sentences."
  20)

(defn export!
  "Every hit of CQP `query` in `corpus` (an uppercase CQP corpus name)
  via `ctx` as the rows of an export, the first `:limit` of `opts`
  (the rest as `kwic!` takes them): {:corpus ... :size <matches>
  :annotations [<attr> ...] :rows [[cpos matchend left match right
  <value> ...] ...]}, the values of each row being those of its
  :annotations, the corpus's positional attributes but word and its
  annotated s-attributes, in that order.

  Read from the saved query result when one is stored, which the
  concordance of the same search will have saved, and run afresh and
  saved otherwise (see dk.cst.corpus-probe.search.result/read-stored! and
  dk.cst.corpus-probe.search.result/run-fresh!), so an export costs no
  query after a concordance and warms the cache before one. The context
  is a number of words either side: an export has no room for a unit of
  text (see `export-unit-width`). A narrowing of nothing is answered
  without CQP (see dk.cst.corpus-probe.search.result/narrowing-nothing?).
  Throws ex-info when CQP reports an error, times out or dies."
  [ctx corpus query opts]
  (let [ctx         (corpus/corpus-ctx ctx corpus)
        query       (opts/corpus-query! ctx corpus query (:within opts))
        nothing?    (result/narrowing-nothing? ctx corpus query
                                               (dissoc opts :within) size!)
        opts        (update (opts/kwic-opts! ctx corpus query opts) :context
                            #(if (keyword? %) export-unit-width %))
        {:keys [nqr p-attrs struct-attrs]} opts
        annotations (into (vec (remove #{:word} p-attrs)) struct-attrs)]
    (if nothing?
      {:corpus corpus :size 0 :annotations annotations :rows []}
      (let [sections (or (result/read-stored!
                          ctx corpus query opts batch/stored-export-batch
                          #(cache/holds? ctx corpus nqr (result/match-count %)))
                         (result/run-fresh! ctx corpus query opts
                                            batch/export-batch))]
        {:corpus      corpus
         :size        (result/match-count sections)
         :annotations annotations
         ;; the first section is the TAB-free columns of every hit; the
         ;; contexts are trimmed, since a position outside the corpus
         ;; prints as an empty word and the words are joined by spaces
         :rows        (mapv (fn [[line & values]]
                              (into (mapv str/trim (str/split line #"\t" -1))
                                    values))
                            (parse/tabulate->rows (:tabulate sections)))}))))

(defn export-corpora!
  "The exports (see `export!`) of CQP `query` in `corpora` (uppercase
  names, in display order) via `ctx` under `opts`, one after another
  and lazily: each within what is left of `limit` rows after the
  corpora before it, and none once it is spent, and within `deadline`
  (see dk.cst.corpus-probe.cwb/deadline), after which the rest are
  reported as timed out. A corpus whose query fails carries its :error
  instead of its rows, as a corpus of `concordance!` does."
  [ctx corpora query deadline limit opts]
  (lazy-seq
   (when-let [[corpus & more] (and (pos? limit) (seq corpora))]
     (let [res (if (cwb/overdue? deadline)
                 {:corpus corpus :error {:type :timeout}}
                 (cwb/attempt corpus
                              #(export! (cwb/within-deadline ctx deadline)
                                        corpus query
                                        (assoc opts :limit limit))))]
       (cons res (export-corpora! ctx more query deadline
                                  (- limit (count (:rows res))) opts))))))

(defn fill-page!
  "Run `query` in `corpora` one at a time via `ctx` until the `rows`
  [from to] of the combined result are filled or `deadline` passes.

  Each corpus contributes the rows of its own result that fall in the
  range, offset by the sizes of the corpora before it, and its count map
  (as from `corpus-size!`), remembered for `size!` (see
  `remember-size!`); a corpus that fails contributes no rows.
  `opts` are the display options of `kwic!`. Returns {:counts [...] :hits
  [...] :remaining [corpus ...]} where :remaining are the corpora not
  queried."
  [ctx corpora query [from to] deadline opts]
  (loop [[corpus & more :as remaining] corpora
         offset 0
         counts []
         hits   []]
    (if (or (nil? corpus) (> offset to) (cwb/overdue? deadline))
      {:counts counts :hits hits :remaining remaining}
      (let [rows [(max 0 (- from offset)) (- to offset)]
            res  (cwb/attempt
                  corpus
                  (fn []
                    (let [res (kwic! (cwb/within-deadline ctx deadline)
                                     corpus query (assoc opts :rows rows))]
                      ;; the count of the whole search asks for this
                      ;; corpus again, and the page has just counted it
                      (remember-size! ctx corpus query opts (:size res))
                      res)))]
        (recur more
               (+ offset (:size res 0))
               (conj counts (assoc (select-keys res [:size :error])
                                   :corpus corpus))
               (into hits (map #(assoc % :corpus corpus)) (:hits res)))))))

(defn concordance!
  "Run CQP `query` against `corpora` (uppercase names, in display order) via
  `ctx` and return one page of the combined concordance: the hits ordered
  by corpus, then in each corpus's own sort order.

  Follows Korp: the corpora are queried one at a time until the requested
  page is filled, then the remaining corpora are only counted, in parallel,
  all within the `deadline` of `ctx`. A corpus whose query fails (an
  attribute it lacks, a timeout) contributes no hits and carries its
  :error instead of its :size, so one bad corpus does not fail the whole
  search.

  `opts` accepts :page and :page-size (see
  dk.cst.corpus-probe.search.batch/page-defaults), :incremental? (see below)
  and the display options of `kwic!` (:context, :sort, :filter, :subset,
  :near, :sample, :within).
  A :sample is drawn per corpus, each being queried on its own, so over
  several corpora it is that many hits from each: what also keeps one
  corpus's saved result independent of which others were searched beside
  it, and what keeps a large corpus from crowding a small one out of the
  sample. Returns {:query ... :context ... :filter ... :subset ... :near ...
  :sample ... :page ... :page-size ... :counts [{:corpus ... :size ...}
  ...] :size <hits in all readable corpora> :hits [hit ...]}, each hit
  tagged with its :corpus. The narrowings and the :sample are reported
  back because a page of a narrowed result is not a page of the whole one
  and nothing else about it says so; the :context as asked for, the unit
  being each corpus's own to name.

  Under a true :incremental? the remaining corpora are not counted here:
  those counted before are reported from memory (see `known-sizes`), the
  rest are named in `:remaining` for a count to be asked for later, and
  the :size is the hits counted so far. `:remaining` is absent once every
  corpus is counted."
  ([ctx corpora query]
   (concordance! ctx corpora query {}))
  ([ctx corpora query opts]
   (let [{:keys [page page-size context filter patterns subset near sample
                 incremental?]
          :as   opts}
         (merge batch/page-defaults opts)
         kwic-opts (dissoc opts :page :page-size :incremental?)
         deadline  (cwb/deadline ctx)
         {:keys [counts hits remaining]}
         (fill-page! ctx corpora query (batch/page-rows page page-size)
                     deadline kwic-opts)
         counts    (into counts (if incremental?
                                  (known-sizes ctx remaining query kwic-opts)
                                  (corpus-sizes! ctx remaining query deadline
                                                 kwic-opts)))
         remaining (when incremental?
                     (not-empty (vec (remove (set (map :corpus counts))
                                             remaining))))]
     (cond-> {:query     query
              :context   context
              :filter    filter
              :patterns  patterns
              :subset    subset
              :near      near
              :sample    sample
              :page      page
              :page-size page-size
              :counts    counts
              :size      (reduce + (keep :size counts))
              :hits      hits}
       remaining (assoc :remaining remaining)))))
