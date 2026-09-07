(ns dk.cst.corpus-probe.search.result
  "The stored-or-fresh engine of a search, shared by its pages, exports
  and breakdowns: one runner for every batch over the result of a query,
  the saved query result read back when there is one and the query run
  and saved otherwise, and the checks on what comes back.

  A saved result is CQP's own (see dk.cst.corpus-probe.search.cache), and
  CQP reports neither a file it cannot read nor a save it could only half
  make, so what is read back is judged before it is served, and a save
  that fails is no reason to fail the search."
  (:require [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.parse :as parse]
            [dk.cst.corpus-probe.search.batch :as batch]
            [dk.cst.corpus-probe.search.cache :as cache]
            [taoensso.telemere :as t]))

(defn run-result!
  "The sections of `batch` ([section command] pairs, see
  dk.cst.corpus-probe.search.batch/batch-sections) run against `corpus`
  via `ctx`, `query` being what they run, which the error names when CQP
  reports one, times out or dies (see dk.cst.corpus-probe.cwb/batch!)."
  [ctx corpus query batch]
  (batch/batch-sections batch (cwb/batch! ctx corpus query
                                          (mapv second batch))))

(defn match-count
  "How many matches the `sections` of a batch report in their `size`."
  [{[size-lines] :size}]
  (parse/size->n size-lines))

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

(defn read-stored!
  "The sections of a batch over the saved query result `:nqr` of `opts`
  in `corpus` via `ctx`, or nil when none is stored or the stored one does
  not read.

  A stored result CQP cannot read is discarded, leaving the caller to run
  `query`: one left from an earlier build of the corpus kills CQP
  outright, one reaped between the check and the read leaves the batch
  reporting an undefined corpus, and a damaged one is caught by `sound?`.
  A timeout is not treated that way and the result is kept, since it says
  the machine is busy rather than that the file is bad.

  `stored-batch` builds the batch that reads the result from the corpus,
  the name and the options, and `sound?` judges its sections: the page
  batch and `intact?` for a page, the export or count batch and the size
  of the file (see dk.cst.corpus-probe.search.cache/holds?) where a whole
  result is read."
  [ctx corpus query {:keys [nqr] :as opts} stored-batch sound?]
  (when (and nqr (cache/stored? ctx corpus nqr))
    (try
      (let [sections (run-result! ctx corpus query
                                  (stored-batch corpus nqr opts))]
        (when-not (sound? sections)
          (throw (ex-info "Stored result read back damaged"
                          {:corpus corpus :error {:type :damaged}})))
        (cache/touch! ctx corpus nqr)
        ;; reads reclaim as well as saves, or a server that has stopped
        ;; saving sits at its high-water mark until it saves again
        (cache/reap-due! ctx)
        sections)
      (catch Exception e
        (when (cwb/timeout? e)
          (throw e))
        (t/event! ::stored-result-discarded
                  {:level :warn
                   :data  {:corpus corpus :error (ex-message e)}})
        (cache/discard! ctx corpus nqr)
        nil))))

(defn run-fresh!
  "The sections of a batch over the result of `query` in `corpus` via
  `ctx`, run afresh and saved under the `:nqr` of `opts` when there is
  one.

  The batch that saves is the batch that does not plus the save, so a
  failure could be either, and CQP reports a directory it cannot write to
  like any other error. When the cache is on, the batch is therefore run
  once more without it before the failure is reported: a cache that has
  stopped working is no reason to stop answering. A timeout is not
  retried, having spent its whole budget already. Both runs get the query
  timeout (see dk.cst.corpus-probe.cwb/running-ctx).

  `fresh-batch` builds the batch from the corpus, the query and the
  options: the page batch for a page (see
  dk.cst.corpus-probe.search.batch/kwic-batch), the export batch for an
  export."
  [ctx corpus query {:keys [nqr] :as opts} fresh-batch]
  (let [pending (when nqr (cache/pending-name nqr))
        run     #(run-result! (cwb/running-ctx ctx) corpus query
                              (fresh-batch corpus query %))]
    (try
      (let [sections (run (assoc opts :nqr pending))]
        (when nqr
          (cache/commit! ctx corpus pending nqr (match-count sections))
          ;; after the save rather than before, so the result just written
          ;; is what the disk budget is measured against
          (cache/reap-due! ctx))
        sections)
      (catch Exception e
        (when (or (nil? nqr) (cwb/timeout? e))
          (throw e))
        (let [sections (run (dissoc opts :nqr :cache-dir))]
          ;; logged only once the retry has answered, which is what says
          ;; the cache was at fault rather than the query
          (t/event! ::cache-bypassed
                    {:level :warn
                     :data  {:corpus corpus :error (ex-message e)}})
          sections)))))

(defn narrowing-nothing?
  "True when `opts` narrow a result of `query` in `corpus` via `ctx` (see
  dk.cst.corpus-probe.cwb.command/narrowing) that is empty before one of
  the narrowings runs: nothing to narrow. `f` counts the matches of a
  query in a corpus under narrowing options, remembering every count, as
  dk.cst.corpus-probe.search/size! does.

  CQP cannot be asked to narrow nothing. `set keyword` on an empty
  result is an error and `subset` on one fails an assertion, so a query
  finding nothing, or a corpus the metadata filter leaves no region in,
  failed as soon as it was narrowed, and so did a result the first
  narrowing emptied for the second. So the result each narrowing starts
  from is counted first, with the narrowings before it and nothing
  else. The count before any narrowing is the one the search being
  narrowed already made, and `f` remembers every count, so asking is
  usually free."
  [ctx corpus query opts f]
  (let [steps (mapv first (command/narrowing opts))]
    (boolean
     (some (fn [i]
             (zero? (f ctx corpus query
                       (apply dissoc opts :sample (drop i steps)))))
           (range (count steps))))))
