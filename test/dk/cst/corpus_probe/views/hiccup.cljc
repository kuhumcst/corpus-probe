(ns dk.cst.corpus-probe.views.hiccup
  "Test helper for inspecting hiccup.")

(defn deep
  "All nodes of hiccup `form`, descending into attribute maps too, so tests
  can look for attribute values as well as tags and text."
  [form]
  (tree-seq coll? seq form))
