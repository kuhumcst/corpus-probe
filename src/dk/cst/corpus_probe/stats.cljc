(ns dk.cst.corpus-probe.stats
  "Statistics computed from search results, shared by the views and the
  exports so both report the same numbers.")

(defn per-million
  "Frequency `n` per million tokens of a corpus of `tokens`, to one
  decimal; nil for an empty corpus."
  [n tokens]
  (when (pos? tokens)
    (/ (Math/round (* 10.0 (/ (* n 1000000.0) tokens))) 10.0)))
