(ns dk.cst.corpus-probe.views.hiccup
  "Test helpers for the views: hiccup inspection and the two lookup
  contexts the views translate through."
  (:require [dk.cst.corpus-probe.i18n :as i18n]))

(defn deep
  "All nodes of hiccup `form`, descending into attribute maps too, so tests
  can look for attribute values as well as tags and text."
  [form]
  (tree-seq coll? seq form))

(def en
  "The lookup context of the source language, in which every string is
  its own msgid."
  (i18n/->ui "en"))

(def da
  "The lookup context of the bundled Danish translation."
  (i18n/->ui "da"))
