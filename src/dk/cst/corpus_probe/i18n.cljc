(ns dk.cst.corpus-probe.i18n
  "The Danish and English user interface: the gettext tables the views
  render their strings through, read from the PO files under
  resources/i18n. Every UI string is written in the source in English,
  which is its own key, so a string no translation covers falls back to
  readable English. Only the interface is translated: corpus content,
  CQP's messages and the exports' column names are shown as they are."
  (:require [clojure.string :as str]
            #?(:clj [dk.cst.corpus-probe.i18n.po :as po]))
  #?(:cljs (:require-macros
            [dk.cst.corpus-probe.i18n.po :refer [inline-tables]])))

(def source-language
  "The language the msgids are written in, which therefore needs no
  table of its own."
  "en")

(def tables
  "The translation tables by language code. Read from the classpath on
  the server and inlined at compile time in the browser, which has no
  filesystem to read them from."
  #?(:clj  (po/tables)
     :cljs (inline-tables)))

(def languages
  "The supported UI language codes, in display order: the source
  language and everything a PO file translates it into."
  (vec (sort (conj (set (keys tables)) source-language))))

(def default-language
  "The language served when the request asks for none we have: Danish,
  the language of the readers this replaces KORP for."
  "da")

(defn supported?
  "True when `lang` is one of the `languages`."
  [lang]
  (boolean (some #{lang} languages)))

(defn ->ui
  "The lookup context for language code `lang`: the code itself, which
  decides how numbers are written, and the translation table, which is
  empty for the `source-language` and for a language we do not have."
  [lang]
  {:lang lang :table (get tables lang {})})

(defn fill
  "Replace each `{key}` in `s` with the value under that key in
  `values`, a key the map lacks left as it stands; a value goes in as it
  is, so a number that wants its digits grouped is formatted first (see
  `group-digits`).

  (fill \"token {n}\" {:n 2})
  ;; => \"token 2\""
  [s values]
  (str/replace s #"\{(\w+)\}"
               (fn [[whole k]] (str (get values (keyword k) whole)))))

(defn tr
  "The translation of English UI string `s` under `ui`, or `s` itself,
  its placeholders filled from `values` when given (see `fill`).

  (tr (->ui \"da\") \"Search\")
  ;; => \"Søgning\""
  ([ui s]
   (get (:table ui) s s))
  ;; the lookup again rather than `(tr ui s)`: the scanner reads a call
  ;; of `tr` on a symbol as a lookup of no literal string, and warns
  ([ui s values]
   (fill (get (:table ui) s s) values)))

(defn trx
  "The translation of English UI string `s` in the disambiguating
  `context` under `ui`, or `s` itself: gettext's answer to one English
  word that several languages split.

  (trx (->ui \"da\") \"button\" \"Search\")
  ;; => \"Søg\"   (the button; the page heading is \"Søgning\")"
  [ui context s]
  ;; the context is part of the msgid, the PO reader having no msgctxt
  (get (:table ui) (str context "|" s) s))

(defn trn
  "The translation of English singular `s1` or plural `s2` for the
  count `n` under `ui`, its `{n}` filled from `values` when given.

  (trn (->ui \"da\") \"region\" \"regions\" 2)
  ;; => \"regioner\""
  ([ui s1 s2 n]
   (let [[one many] (get (:table ui) [s1 s2] [s1 s2])]
     ;; the rule Danish and English share; a language dividing them
     ;; otherwise needs its own, and a PO reader keeping more forms
     (if (= 1 n) one many)))
  ;; the lookup again rather than `(trn ui s1 s2 n)`, as in `tr`
  ([ui s1 s2 n values]
   (let [[one many] (get (:table ui) [s1 s2] [s1 s2])]
     (fill (if (= 1 n) one many) values))))

(def number-formats
  "How each language writes a number: its thousands and decimal
  separators, which Danish and English swap. Not translations, since a
  msgid of `.` says nothing to a translator."
  {"da" {:group "." :decimal ","}
   "en" {:group "," :decimal "."}})

(defn fixed
  "Number `n` written with `decimals` digits after the point, whatever
  the platform: a JVM double prints its .0 and a JavaScript number does
  not."
  [n decimals]
  #?(:clj  (.toPlainString (.setScale (bigdec n) (int decimals)
                                      java.math.RoundingMode/HALF_UP))
     :cljs (.toFixed n decimals)))

(defn group-digits
  "Write number `n` the way `ui`'s language writes it: its digits
  grouped in thousands, its fraction after the decimal separator, with
  exactly `decimals` digits of it when given (see `fixed`); nil for nil,
  so a statistic that could not be computed shows as nothing.

  (group-digits (->ui \"da\") 1234.5)
  ;; => \"1.234,5\""
  ([ui n]
   (group-digits ui n nil))
  ([{:keys [lang] :as ui} n decimals]
   (when (some? n)
     (let [{:keys [group decimal]} (number-formats lang (number-formats
                                                         source-language))
           [whole fraction] (str/split (if decimals (fixed n decimals) (str n))
                                       #"\.")]
       (cond-> (->> (reverse whole)
                    (partition-all 3)
                    (map (comp str/join reverse))
                    (reverse)
                    (str/join group))
         fraction (str decimal fraction))))))

(comment
  ;; a string no table covers falls back to its own English
  (tr (->ui "da") "Nonesuch")
  ;; => "Nonesuch"

  (group-digits (->ui "en") 1234.5)
  ;; => "1,234.5"
  #_.)
