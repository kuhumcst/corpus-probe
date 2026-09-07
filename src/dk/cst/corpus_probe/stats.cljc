(ns dk.cst.corpus-probe.stats
  "The arithmetic of a frequency table, shared by the views and the
  exports so both report the same numbers: which corpora could be
  counted, what a frequency is measured against, and the rate per
  million of it.")

(defn per-million
  "Frequency `n` per million tokens of a corpus of `tokens`, to one
  decimal; nil for an empty corpus."
  [n tokens]
  (when (pos? tokens)
    (/ (Math/round (* 10.0 (/ (* n 1000000.0) tokens))) 10.0)))

(defn readable-counts
  "The `counts` of a frequency result (see
  dk.cst.corpus-probe.search.frequency/frequency-table!) whose corpus could be
  counted: the ones carrying its tokens; the rest report a failure."
  [counts]
  (filter :tokens counts))

(defn tokens
  "The tokens of every corpus of `counts` that could be counted (see
  `readable-counts`), summed: what a total is measured against."
  [counts]
  (reduce + (map :tokens (readable-counts counts))))

(defn total?
  "True when `counts` hold several corpora that could be counted (see
  `readable-counts`), so that a table shows their total beside them."
  [counts]
  (boolean (next (readable-counts counts))))

(defn row-tokens
  "The tokens the frequency of `row` is measured against: `tokens`, the
  corpus's own or every corpus's, unless the result is `sized`, in which
  case the text of the value is what was searched, so its own tokens
  count: `corpus`'s share of the row's, or all of them without a corpus."
  ([sized tokens row]
   (if sized (reduce + (vals (:tokens row))) tokens))
  ([sized tokens corpus row]
   (if sized (get (:tokens row) corpus 0) tokens)))

(defn row-docs
  "The texts the value of `row` occurs in: in `corpus`, or in every
  corpus without one."
  ([row]
   (reduce + (vals (:docs row))))
  ([corpus row]
   (get (:docs row) corpus 0)))
