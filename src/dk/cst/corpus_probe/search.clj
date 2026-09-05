(ns dk.cst.corpus-probe.search
  "High-level search operations: one function call in, plain data out.

  Composes command generation (dk.cst.corpus-probe.commands), the
  child-process driver (dk.cst.corpus-probe.cqp) and the output parsers
  (dk.cst.corpus-probe.parse) into complete round trips: a KWIC page or a
  match count for one corpus, and a concordance over several. Breakdowns
  of those matches are dk.cst.corpus-probe.frequency, composed on the
  corpus context, the bounded fan-out and the collator this owns.

  Every search takes a :filter option, a metadata filter, and a :patterns
  option, the regexes its values may match instead (see
  dk.cst.corpus-probe.commands/filter-query), restricting it to the matching
  regions of each corpus.

  These functions are the trust boundary for the web layer: only the CQP
  query itself is protected by the QueryLock sandbox, so every other
  parameter spliced into a command (corpus names and attribute names) is
  validated here against the corpus's own inventory first."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cache :as cache]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.parse :as parse]
            [dk.cst.corpus-probe.commands :as commands]
            [dk.cst.corpus-probe.tools :as tools]
            [taoensso.telemere :as t])
  (:import [java.text Collator]
           [java.util Locale]))

(defn corpus-ctx
  "Return `ctx` configured for `corpus`: validates the corpus name (it is
  spliced into commands outside the QueryLock sandbox) and sets the
  corpus's own charset for the CQP round trip."
  [ctx corpus]
  (commands/valid-corpus-name corpus)
  (assoc ctx :charset (corpus/charset ctx corpus)))

(defn error-map
  "The error map for exception `e` thrown by a search: the CQP error it
  carries; a :rejected error with the message of one of this project's own
  guards (an ex-info without a CQP error); or an :internal error for
  anything else, whose details are logged rather than shown, since an
  exception message may name a server path."
  [e]
  (or (:error (ex-data e))
      (if (instance? clojure.lang.ExceptionInfo e)
        {:type :rejected :message (ex-message e)}
        (do (t/error! ::search-failed e)
            {:type :internal}))))

(defn pmap-n
  "Map `f` over `coll` with at most `n` calls running at once, in order.

  `pmap` alone starts a whole chunk of 32 futures at once, and each call
  here spawns a process, so the fan-out is bounded instead."
  [n f coll]
  (mapcat #(doall (pmap f %)) (partition-all n coll)))

(defn parallelism
  "How many corpora `ctx` queries at once (its :parallelism, default 8)."
  [ctx]
  (:parallelism ctx 8))

(defn deadline
  "The wall-clock deadline (a millisecond timestamp) of a search started
  now under `ctx`: its :search-budget-ms (default 60000) from now, after
  which no further corpus is queried, so a query that times out in every
  corpus cannot hold a request for the sum of all the timeouts."
  [{:keys [search-budget-ms] :or {search-budget-ms 60000} :as ctx}]
  (+ (System/currentTimeMillis) search-budget-ms))

(defn overdue?
  "True once `deadline` (see `deadline`) has passed."
  [deadline]
  (> (System/currentTimeMillis) deadline))

(defn attr-names
  "The names of the `attributes` matching `pred`."
  [pred attributes]
  (->> (filter pred attributes)
       (mapv :name)))

(defn annotated-s-attr?
  "True when attribute description `m` is an s-attribute carrying values."
  [{:keys [type values?] :as m}]
  (and (= :structural type) values?))

(def units
  "The names a unit of text goes by among a corpus's s-attributes, in the
  order they are looked for: a sentence is `s` in CWB's own corpora and
  `sentence` in the KU ones, a paragraph `p` or `paragraph`."
  {:sentence  [:s :sentence]
   :paragraph [:p :paragraph]
   :text      [:text]})

(defn unit-attr
  "The s-attribute among `attributes` (descriptions as
  dk.cst.corpus-probe.corpus/attributes! reports them) marking `unit`, a
  key of `units`; nil when the corpus marks none."
  [attributes unit]
  (some (set (attr-names #(= :structural (:type %)) attributes))
        (units unit)))

(defn within-attr!
  "The s-attribute of `corpus` via `ctx` that a search kept within `unit`
  (see `units`) is restricted to; nil for no unit, and for a corpus that
  does not mark it, where the search runs unrestricted rather than not at
  all."
  [ctx corpus unit]
  (when unit
    (unit-attr (corpus/attributes! ctx corpus) unit)))

(defn corpus-query!
  "CQP `query` as `corpus` via `ctx` runs it: its sentence tags named
  after the corpus's own sentence attribute (see
  dk.cst.corpus-probe.commands/sentence-tags), a within clause of its
  own likewise, or dropped where the corpus marks no such unit (see
  dk.cst.corpus-probe.commands/within-clause), and kept within `unit`
  (see `within-attr!`; nil for no unit)."
  [ctx corpus query unit]
  (let [attributes (corpus/attributes! ctx corpus)
        attr       #(unit-attr attributes %)]
    (-> query
        (commands/sentence-tags (attr :sentence))
        (commands/within-clause (into {}
                                      (map (juxt identity attr))
                                      (keys units)))
        (commands/within-query (attr unit)))))

(defn countable-attr?
  "True when attribute description `m` is one whose values a result can
  be counted or narrowed by: a positional attribute, or a structural one
  carrying values."
  [m]
  (or (= :positional (:type m)) (annotated-s-attr? m)))

(defn corpus-subset!
  "The narrowing `subset` (see dk.cst.corpus-probe.commands/subset-command)
  as `corpus` via `ctx` may run it: with its attribute checked against
  the corpus's countable attributes (see `countable-attr?`), since the
  name is spliced into a command outside the QueryLock; nil for none."
  [ctx corpus {:keys [attr] :as subset}]
  (when subset
    (when-not (some #(and (= attr (:name %)) (countable-attr? %))
                    (corpus/attributes! ctx corpus))
      (throw (ex-info "Not an attribute of this corpus"
                      {:corpus corpus :attr attr})))
    subset))

(defn corpus-sort!
  "The sort mode `mode` (see dk.cst.corpus-probe.commands/sort-command) as
  `corpus` via `ctx` may run it: with the positional attribute it names,
  if any (see dk.cst.corpus-probe.commands/sort-attr), checked against the
  corpus's inventory, since the name is spliced into a command outside
  the QueryLock."
  [ctx corpus mode]
  (when-let [attr (commands/sort-attr mode)]
    (when-not (some #(and (= attr (:name %)) (= :positional (:type %)))
                    (corpus/attributes! ctx corpus))
      (throw (ex-info "Not a positional attribute of this corpus"
                      {:corpus corpus :attr attr}))))
  mode)

(defn corpus-context
  "The width of context a corpus with `attributes` shows for `context`
  (see dk.cst.corpus-probe.commands/context-spec): a number of words as it
  is, and a unit of text (a key of `units`) as the corpus's own attribute
  for it, or as the default width where the corpus marks no such unit,
  since a hit shown with the usual context beats one not shown at all."
  [attributes context]
  (if (keyword? context)
    (or (unit-attr attributes context) (:context commands/kwic-defaults))
    context))

(defn corpus-filter!
  "Metadata `filter` (a map of attribute to the set of values accepted)
  and `patterns` (a map of attribute to the regexes accepted, see
  dk.cst.corpus-probe.api/pattern-params) as
  dk.cst.corpus-probe.commands/filter-query takes them for `corpus` via
  `ctx`: [attr values patterns] triples, the attribute with the most
  regions first, that being the innermost one the filter query must
  anchor on; nil when neither restricts anything.

  Every attribute must be an annotated s-attribute of the corpus, by the
  cached describe statistics that also count its regions; anything else
  is rejected, since the names are spliced into a command, sandboxed
  though the filter query is."
  [ctx corpus filter patterns]
  (when (or (seq filter) (seq patterns))
    (let [regions (into {}
                        (keep (fn [{:keys [name regions values?]}]
                                (when values? [name regions])))
                        (:s-attrs (tools/describe-corpus! ctx corpus)))
          attrs   (distinct (concat (keys filter) (keys patterns)))]
      (when-let [bad (seq (remove regions attrs))]
        (throw (ex-info "Not an annotated structural attribute of this corpus"
                        {:corpus corpus :attrs bad})))
      (vec (sort-by (juxt (comp - regions first) first)
                    (for [attr attrs]
                      [attr (get filter attr #{}) (get patterns attr)]))))))

(defn cache-opts
  "`opts` with the cache directory and the name the result of `query` in
  `corpus` is saved under, when `ctx` keeps a cache and `opts` does not
  turn it off with a false :cache?.

  Creates the corpus's cache directory, CQP given one that does not exist
  saving nothing and reporting nothing. The name is taken from the filter
  the corpus was given rather than the one the request asked for, so that
  two spellings of one filter share a saved result."
  [ctx corpus query opts]
  (if-let [dir (and (:cache? opts true)
                    (cache/corpus-directory! ctx corpus))]
    (assoc opts
           :cache-dir (str dir)
           :nqr       (cache/result-name ctx corpus query opts))
    opts))

(defn kwic-opts!
  "The options one KWIC batch for `corpus` needs, from the `opts` of
  `kwic!` via `ctx`.

  The KWIC defaults, the corpus's positional attributes and the structural
  attributes to fetch per hit, its context width as the corpus shows it
  (see `corpus-context`), its metadata filter as
  dk.cst.corpus-probe.commands/filter-query takes it, its narrowing and its
  sort mode checked (see `corpus-subset!` and `corpus-sort!`), and
  whatever the cache adds (see `cache-opts`). Requested structural
  attributes are checked against the corpus's inventory first, since
  their names are spliced into a command."
  [ctx corpus query opts]
  (let [attributes (corpus/attributes! ctx corpus)
        annotated  (attr-names annotated-s-attr? attributes)
        requested  (:struct-attrs opts)
        opts       (merge commands/kwic-defaults opts)]
    (when-let [bad (seq (remove (set annotated) requested))]
      (throw (ex-info "Unknown struct attributes"
                      {:corpus corpus :struct-attrs bad})))
    (cache-opts ctx corpus query
                (assoc opts
                       :p-attrs      (attr-names #(= :positional (:type %))
                                                 attributes)
                       :struct-attrs (or requested annotated)
                       :context      (corpus-context attributes
                                                     (:context opts))
                       :filter       (corpus-filter! ctx corpus
                                                     (:filter opts)
                                                     (:patterns opts))
                       :subset       (corpus-subset! ctx corpus
                                                     (:subset opts))
                       :sort         (corpus-sort! ctx corpus
                                                   (:sort opts))))))

(defn run-kwic-batch!
  "Run KWIC `batch` for `query` in `corpus` via `ctx` and return its
  sections (see dk.cst.corpus-probe.commands/batch-sections).

  Throws ex-info when CQP reports an error, times out or dies."
  [ctx corpus query batch]
  (let [{:keys [results error]} (cqp/run-batch! ctx (mapv second batch))]
    (when error
      (throw (ex-info "KWIC query failed"
                      {:corpus corpus :query query :error error})))
    (commands/batch-sections batch results)))

(defn batch-matches
  "How many matches the `sections` of a KWIC batch report."
  [{[size-lines] :size}]
  (parse-long (first size-lines)))

(defn intact?
  "True when the `sections` of a KWIC batch describe a page that could
  have come from a real query result.

  A truncated save file is not reported as an error when it is read: the
  pages past the cut are zero-filled, so every row in them comes back at
  corpus position 0. No two matches of one query share a position, in any
  sort order, so a page whose positions repeat did not come from the
  result it claims to."
  [{[dump-lines] :dump}]
  (let [positions (map :match (parse/dump->anchors dump-lines))]
    (or (empty? positions) (apply distinct? positions))))

(defn timeout?
  "True when exception `e` reports that CQP was killed for taking too long
  rather than answering."
  [e]
  (= :timeout (get-in (ex-data e) [:error :type])))

(defn stored-sections!
  "The sections of one KWIC page read from the saved query result `:nqr`
  of `opts`, or nil when none is stored or the stored one does not read.

  A stored result CQP cannot read is discarded, leaving the caller to run
  the query: one left from an earlier build of the corpus kills CQP
  outright, one reaped between the check and the read leaves the batch
  reporting an undefined corpus, and a truncated one is caught by
  `intact?`. A timeout is not treated that way and the result is kept,
  since it says the machine is busy rather than that the file is bad.

  `stored-batch` builds the batch that reads the result from the corpus,
  the name and the options, and `sound?` judges its sections: the page
  batch and `intact?` for a page (see `kwic-sections!`), the export
  batch and the size of the file for `export!`."
  [ctx corpus query {:keys [nqr] :as opts} stored-batch sound?]
  (when (and nqr (cache/stored? ctx corpus nqr))
    (try
      (let [batch    (stored-batch corpus nqr opts)
            sections (run-kwic-batch! ctx corpus query batch)]
        (when-not (sound? sections)
          (throw (ex-info "Stored result read back damaged"
                          {:corpus corpus :error {:type :damaged}})))
        (cache/touch! ctx corpus nqr)
        ;; reads reclaim as well as saves, or a server that has stopped
        ;; saving sits at its high-water mark until it saves again
        (cache/reap-due! ctx)
        sections)
      (catch Exception e
        (when (timeout? e)
          (throw e))
        (t/event! ::stored-result-discarded
                  {:level :warn
                   :data  {:corpus corpus :error (ex-message e)}})
        (cache/discard! ctx corpus nqr)
        nil))))

(defn running-ctx
  "`ctx` with the longer timeout a batch that runs the query needs (its
  :query-timeout-ms), leaving batches that only read a result already
  saved on the ordinary :timeout-ms.

  The two are orders of magnitude apart, the figures being in
  resources/config.edn beside the settings. Counting and displaying run
  the same query, so they get the same budget: giving counting less would
  let a corpus time out while being counted and succeed while being shown,
  and a count that fails is left out of the total, quietly costing the
  search pages of hits it has."
  [ctx]
  (cond-> ctx
    (:query-timeout-ms ctx) (assoc :timeout-ms (:query-timeout-ms ctx))))

(defn within-deadline
  "`ctx` with its timeouts cut down to the time left before `deadline`.

  Without this, :search-budget-ms bounds only how many corpora a search
  starts, not how long the last one may run: one started just before the
  deadline would get a whole timeout of its own on top of the budget, and
  the retry in `fresh-sections!` another."
  [ctx deadline]
  (let [left (max 1000 (- deadline (System/currentTimeMillis)))]
    (cond-> ctx
      (:timeout-ms ctx)       (update :timeout-ms min left)
      (:query-timeout-ms ctx) (update :query-timeout-ms min left))))

(defn fresh-sections!
  "The sections of one KWIC page of `query` in `corpus` via `ctx`, run
  afresh and saved under the `:nqr` of `opts` when there is one.

  The batch that saves is the batch that does not plus the save, so a
  failure could be either, and CQP reports a directory it cannot write to
  like any other error. When the cache is on, the batch is therefore run
  once more without it before the failure is reported: a cache that has
  stopped working is no reason to stop answering. A timeout is not
  retried, having spent its whole budget already. Both runs get the query
  timeout (see `running-ctx`).

  `fresh-batch` builds the batch from the corpus, the query and the
  options: the page batch for a page (see
  dk.cst.corpus-probe.commands/kwic-batch and `kwic-sections!`), the
  export batch for `export!`."
  [ctx corpus query {:keys [nqr] :as opts} fresh-batch]
  (let [pending (when nqr (cache/pending-name nqr))
        run     #(run-kwic-batch! (running-ctx ctx) corpus query
                                  (fresh-batch corpus query %))]
    (try
      (let [sections (run (assoc opts :nqr pending))]
        (when nqr
          (cache/commit! ctx corpus pending nqr (batch-matches sections))
          ;; after the save rather than before, so the result just written
          ;; is what the disk budget is measured against
          (cache/reap-due! ctx))
        sections)
      (catch Exception e
        (when (or (nil? nqr) (timeout? e))
          (throw e))
        (let [sections (run (dissoc opts :nqr :cache-dir))]
          ;; logged only once the retry has answered, which is what says
          ;; the cache was at fault rather than the query
          (t/event! ::cache-bypassed
                    {:level :warn
                     :data  {:corpus corpus :error (ex-message e)}})
          sections)))))

(defn kwic-sections!
  "The sections of one KWIC page of `query` in `corpus` via `ctx` under
  `opts` (see `kwic-opts!`): read from the saved query result when one is
  stored (see `stored-sections!`), and run afresh otherwise (see
  `fresh-sections!`).

  Requests that want the same page of the same search share one run of it
  while it is in flight (see dk.cst.corpus-probe.cache/share!), so that a
  reader who reloads during a long sort waits for the sort already running
  rather than starting a second one. They want the same page when they
  would run the same batch, so the batch is the key: anything a future
  option changes about the output changes it too, without anyone having to
  remember to add it."
  [ctx corpus query {:keys [nqr] :as opts}]
  (let [fetch #(or (stored-sections! ctx corpus query opts
                                     commands/stored-kwic-batch intact?)
                   (fresh-sections! ctx corpus query opts
                                    commands/kwic-batch))]
    (if nqr
      (cache/share! (commands/stored-kwic-batch corpus nqr opts) fetch)
      (fetch))))

(declare size!)

(defn narrowing-nothing?
  "True when `opts` narrow a result of `query` in `corpus` via `ctx` (see
  dk.cst.corpus-probe.commands/narrowing) that is empty before one of the
  narrowings runs: nothing to narrow.

  CQP cannot be asked to narrow nothing. `set keyword` on an empty
  result is an error and `subset` on one fails an assertion, so a query
  finding nothing, or a corpus the metadata filter leaves no region in,
  failed as soon as it was narrowed, and so did a result the first
  narrowing emptied for the second. So the result each narrowing starts
  from is counted first, with the narrowings before it and nothing
  else. The count before any narrowing is the one the search being
  narrowed already made, and `size!` remembers every count, so asking
  is usually free."
  [ctx corpus query opts]
  (let [steps (mapv first (commands/narrowing opts))]
    (boolean
     (some (fn [k]
             (zero? (size! ctx corpus query
                           (apply dissoc opts :sample (drop k steps)))))
           (range (count steps))))))

(defn kwic!
  "Run CQP `query` against `corpus` (an uppercase CQP corpus name) through
  the installation described by `ctx` (see dk.cst.corpus-probe.cqp) and
  return the hits in one row range as data.

  Returns {:corpus ... :query ... :size <total hits> :rows [from to]
  :hits [hit ...]} where each hit combines the parsed KWIC line (:cpos :left
  :match :right), its anchors from `dump` (:anchors) and its structural
  metadata (:structs). `opts` accepts :rows (the [from to] row range, see
  dk.cst.corpus-probe.commands/page-rows; default the first page), :context
  (a number of tokens, or a unit of text as `corpus-context` resolves
  it), :sort (a sort mode, or a positional attribute to sort by; see
  `corpus-sort!`), :filter (a metadata filter), :sample
  (how many of the matches to keep, drawn at random; see
  dk.cst.corpus-probe.commands/sample-command), :near (a word the matches
  must have nearby, marked as their keyword; see
  dk.cst.corpus-probe.commands/near-command), :within (a unit of text the
  matches are kept within, see `within-attr!`) and :struct-attrs
  (defaults to every annotated s-attribute of the corpus; anything not in
  that inventory is rejected).

  When `ctx` keeps a cache (see dk.cst.corpus-probe.cache), the result is
  saved there and a later page of the same query, filter and sort mode is
  read back from it rather than queried and sorted again; a false :cache?
  in `opts` leaves it out of the cache, for a query nothing will ask for
  twice. A narrowing of nothing is answered without CQP (see
  `narrowing-nothing?`).

  Throws ex-info when CQP reports an error, times out or dies."
  ([ctx corpus query]
   (kwic! ctx corpus query {}))
  ([ctx corpus query opts]
   (let [ctx   (corpus-ctx ctx corpus)
         query (corpus-query! ctx corpus query (:within opts))]
     (if (narrowing-nothing? ctx corpus query (dissoc opts :within))
       {:corpus corpus
        :query  query
        :size   0
        :rows   (:rows opts (:rows commands/kwic-defaults))
        :hits   []}
       (let [opts     (kwic-opts! ctx corpus query opts)
             {:keys [p-attrs struct-attrs rows]} opts
             sections (kwic-sections! ctx corpus query opts)
             {[cat-lines]  :cat
              [dump-lines] :dump
              tab-sections :tabulate} sections
             hits     (parse/kwic->hits p-attrs cat-lines)
             anchors  (parse/dump->anchors dump-lines)
             structs  (when (seq struct-attrs)
                        ;; one tabulate section per attribute: a whole line
                        ;; is one annotation value, so embedded TABs survive
                        (apply mapv
                               (fn [& values]
                                 (zipmap struct-attrs values))
                               tab-sections))]
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
          :size   (batch-matches sections)
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

  A text is a region of the corpus's own text attribute (see `units`),
  read as one match with no context (see
  dk.cst.corpus-probe.commands/text-batch); a corpus marking no texts has
  none to read, and says so with a :no-texts error. Throws ex-info when
  CQP reports an error, times out or dies."
  [ctx corpus cpos]
  (let [ctx        (corpus-ctx ctx corpus)
        attributes (corpus/attributes! ctx corpus)
        text       (or (unit-attr attributes :text)
                       (throw (ex-info "This corpus marks no texts"
                                       {:corpus corpus
                                        :error  {:type :no-texts}})))
        unit       (or (unit-attr attributes :paragraph)
                       (unit-attr attributes :sentence))
        query      (str (commands/position-query cpos cpos) " expand to "
                        (name text))
        annotated  (attr-names annotated-s-attr? attributes)
        batch      (commands/text-batch corpus query
                                     {:p-attrs      [:word]
                                      :struct-attrs annotated
                                      :shown        (some-> unit vector)})
        {[cat-lines] :cat [dump-lines] :dump tab-sections :tabulate}
        (run-kwic-batch! (running-ctx ctx) corpus query batch)]
    (when-let [hit (first (parse/kwic->hits [:word] cat-lines))]
      (let [{:keys [match matchend]} (first (parse/dump->anchors dump-lines))]
        {:corpus  corpus
         :from    match
         :to      matchend
         :structs (zipmap annotated (map first tab-sections))
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
  saved otherwise (see `stored-sections!` and `fresh-sections!`), so an
  export costs no query after a concordance and warms the cache before
  one. The context is a number of words either side: an export has no
  room for a unit of text (see `export-unit-width`). A narrowing of
  nothing is answered without CQP (see `narrowing-nothing?`). Throws
  ex-info when CQP reports an error, times out or dies."
  [ctx corpus query opts]
  (let [ctx         (corpus-ctx ctx corpus)
        query       (corpus-query! ctx corpus query (:within opts))
        nothing?    (narrowing-nothing? ctx corpus query
                                        (dissoc opts :within))
        opts        (update (kwic-opts! ctx corpus query opts) :context
                            #(if (keyword? %) export-unit-width %))
        {:keys [nqr p-attrs struct-attrs]} opts
        annotations (into (vec (remove #{:word} p-attrs)) struct-attrs)]
    (if nothing?
      {:corpus corpus :size 0 :annotations annotations :rows []}
      (let [sections (or (stored-sections! ctx corpus query opts
                                           commands/stored-export-batch
                                           #(cache/holds? ctx corpus nqr
                                                          (batch-matches %)))
                         (fresh-sections! ctx corpus query opts
                                          commands/export-batch))
            [fixed & structs] (:tabulate sections)]
        {:corpus      corpus
         :size        (batch-matches sections)
         :annotations annotations
         ;; the contexts are trimmed: a position outside the corpus prints
         ;; as an empty word, and the words are joined by spaces
         :rows        (apply mapv
                             (fn [line & values]
                               (into (mapv str/trim
                                           (str/split line #"\t" -1))
                                     values))
                             fixed structs)}))))

(defn export-corpora!
  "The exports (see `export!`) of CQP `query` in `corpora` (uppercase
  names, in display order) via `ctx` under `opts`, one after another
  and lazily: each within what is left of `limit` rows after the
  corpora before it, and none once it is spent, and within `deadline`
  (see `deadline`), after which the rest are reported as timed out. A
  corpus whose query fails carries its :error instead of its rows, as a
  corpus of `concordance!` does."
  [ctx corpora query deadline limit opts]
  (lazy-seq
   (when-let [[corpus & more] (and (pos? limit) (seq corpora))]
     (let [res (if (overdue? deadline)
                 {:corpus corpus :error {:type :timeout}}
                 (try (export! (within-deadline ctx deadline) corpus query
                               (assoc opts :limit limit))
                      (catch Exception e
                        {:corpus corpus :error (error-map e)})))]
       (cons res (export-corpora! ctx more query deadline
                                  (- limit (count (:rows res))) opts))))))

(defn run-size!
  "Count the matches of CQP `query` in `corpus` via `ctx` under `opts`
  (its :filter as `corpus-filter!` returns one, its :subset and :near as
  dk.cst.corpus-probe.commands/narrowing takes them, and its :sample), by
  running the query.

  Throws ex-info when CQP reports an error, times out or dies."
  [ctx corpus query {:keys [filter sample] :as opts}]
  (let [sampling (commands/sample-command sample)
        commands (-> [(str corpus ";")
                      (commands/restricted-query query filter)]
                     (into (map second (commands/narrowing opts)))
                     (cond-> sampling (conj sampling))
                     (conj "size Last;"))
        {:keys [results error]} (cqp/run-batch! ctx commands)]
    (when error
      (throw (ex-info "Size query failed"
                      {:corpus corpus :query query :error error})))
    (parse-long (first (last results)))))

(defn size-args!
  "The arguments a count of CQP `query` in `corpus` via `ctx` under `opts`
  is keyed and run by, as `size!` resolves them for that corpus: the
  corpus's own charset, the query kept within the :within unit (see
  `within-attr!`), and the :filter, :subset, :near and :sample as the
  count takes them. Returns [ctx query opts]."
  [ctx corpus query {:keys [filter patterns sample within near subset]}]
  (let [ctx (corpus-ctx ctx corpus)]
    [ctx
     (corpus-query! ctx corpus query within)
     {:filter (corpus-filter! ctx corpus filter patterns)
      :subset (corpus-subset! ctx corpus subset)
      :near   near
      :sample sample}]))

(defn size!
  "The number of matches of CQP `query` in `corpus` via `ctx`, within the
  :filter of `opts` when there is one, kept within its :within unit (see
  `within-attr!`), narrowed to its :subset and :near (see
  dk.cst.corpus-probe.commands/narrowing) and reduced to its :sample.

  Counted once and then remembered (see
  dk.cst.corpus-probe.cache/count!), because paging a search over several
  corpora counts the ones contributing no rows to the page again on every
  page, and that is a whole query each time. A narrowing of nothing is
  nothing, and is answered without CQP (see `narrowing-nothing?`), but
  remembered like any other count, so that a page can tell it from a
  count still to be made (see `known-size`). Throws ex-info when CQP
  reports an error, times out or dies."
  ([ctx corpus query]
   (size! ctx corpus query {}))
  ([ctx corpus query opts]
   (let [[ctx* query* opts*] (size-args! ctx corpus query opts)]
     (cache/count! ctx* corpus query* opts*
                   #(if (narrowing-nothing? ctx corpus query opts)
                      0
                      (run-size! (running-ctx ctx*) corpus query* opts*))))))

(defn remember-size!
  "Remember `n` as the number of matches of CQP `query` in `corpus` via
  `ctx` under `opts` (as for `size!`).

  A KWIC batch counts the matches it pages through, and the count of the
  whole search asks for that corpus once more, so a page hands its
  count over rather than leaving `size!` to run the query again."
  [ctx corpus query opts n]
  (let [[ctx query opts] (size-args! ctx corpus query opts)]
    (cache/count! ctx corpus query opts (constantly n))))

(defn known-size
  "The number of matches of CQP `query` in `corpus` via `ctx` under `opts`
  (as for `size!`) when it has been counted before, else nil (see
  dk.cst.corpus-probe.cache/known-count). Runs nothing."
  [ctx corpus query opts]
  (let [[ctx query opts] (size-args! ctx corpus query opts)]
    (cache/known-count ctx corpus query opts)))

(defn corpus-size!
  "The size of `query`'s result in `corpus` via `ctx` (`opts` as for
  `size!`) without failing: {:corpus ... :size <n>}, or {:corpus ...
  :error <error map>} when the query cannot run there."
  [ctx corpus query opts]
  (try {:corpus corpus :size (size! ctx corpus query opts)}
       (catch Exception e
         {:corpus corpus :error (error-map e)})))

(defn corpus-sizes!
  "The sizes of `query`'s result in each of `corpora` via `ctx` (`opts`
  as for `size!`), queried in parallel (see `parallelism`) until `deadline`
  passes, after which the rest are reported as timed out. Returns one
  `corpus-size!` map per corpus in the given order."
  [ctx corpora query deadline opts]
  (vec (pmap-n (parallelism ctx)
               (fn [corpus]
                 (if (overdue? deadline)
                   {:corpus corpus :error {:type :timeout}}
                   (corpus-size! (within-deadline ctx deadline)
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

(defn fill-page!
  "Query `corpora` one at a time via `ctx` until the `rows` [from to] of
  the combined result are filled or `deadline` passes.

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
    (if (or (nil? corpus) (> offset to) (overdue? deadline))
      {:counts counts :hits hits :remaining remaining}
      (let [rows [(max 0 (- from offset)) (- to offset)]
            res  (try (let [res (kwic! (within-deadline ctx deadline) corpus
                                       query (assoc opts :rows rows))]
                        ;; the count of the whole search asks for this
                        ;; corpus again, and the page has just counted it
                        (remember-size! ctx corpus query opts (:size res))
                        res)
                      (catch Exception e {:error (error-map e)}))]
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
  dk.cst.corpus-probe.commands/page-defaults), :incremental? (see below)
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
         (merge commands/page-defaults opts)
         kwic-opts (dissoc opts :page :page-size :incremental?)
         deadline  (deadline ctx)
         {:keys [counts hits remaining]}
         (fill-page! ctx corpora query (commands/page-rows page page-size)
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

(defn locale
  "The java.util.Locale named by LC_ALL value `s` (\"da_DK.UTF-8\"), or the
  root locale when it names none.

  A POSIX locale name is its BCP 47 tag with an underscore for the hyphen
  and a charset or modifier suffix, so dropping the suffix and putting the
  hyphen back is the whole conversion."
  [s]
  (-> (str s)
      (str/replace #"[.@].*$" "")
      (str/replace "_" "-")
      (Locale/forLanguageTag)))

(defn ->collator
  "A collator over annotation values in the locale `ctx` sorts in (its
  :sort-locale, see `locale`).

  The values come out of the corpora, and this is the locale CQP itself
  collates them in when it sorts a concordance, so a value list and a
  concordance agree on where æ, ø and å belong. Collators are stateful,
  so each caller gets its own."
  [ctx]
  (Collator/getInstance (locale (:sort-locale ctx))))
