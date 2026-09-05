(ns dk.cst.corpus-probe.i18n
  "The Danish and English user interface: the gettext tables the views
  render their strings through.

  Every UI string is written in the source in English, and that English
  is its own key, so a view reads as the sentence it renders and a
  string no translation covers falls back to readable English rather
  than to an identifier. The translations live in resources/i18n/*.po
  (see dk.cst.corpus-probe.translations), which Poedit and Weblate read
  directly; the template a translator starts from is regenerated from
  the source by dk.cst.corpus-probe.i18n-scan.

  A `ui` is the context the lookups take: the chosen language and its
  table. dk.cst.corpus-probe.views.app derives it once per render and
  hands it down, as the language code used to be handed down. The state
  itself carries only that code: it travels to the client as transit,
  and the client already holds every table.

  Only the interface is translated. CQP's own error messages, attribute
  names, corpus titles and corpus content are shown verbatim, in their
  own language. So are the TSV and CSV exports, whose column names are
  CWB's own and are read by other programs."
  (:require [clojure.string :as str]
            #?(:clj [dk.cst.corpus-probe.translations :as translations]))
  #?(:cljs (:require-macros
            [dk.cst.corpus-probe.translations :refer [inline-tables]])))

(def source-language
  "The language the msgids are written in, which therefore needs no
  table of its own."
  "en")

(def tables
  "The translation tables by language code. Read from the classpath on
  the server and inlined at compile time in the browser, which has no
  filesystem to read them from."
  #?(:clj  (translations/tables)
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
  "`s` with each `{key}` in it replaced by the value under that key in
  `values`: what a translated string takes after lookup, so that a
  translator can put the value where their language wants it. A value
  goes in as it is, so a number that wants its digits grouped is
  formatted by the caller (see `group-digits`); a key the map lacks is
  left as it stands, which is what a translation with a key of its own
  shows.

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
  ([ui s values]
   (fill (tr ui s) values)))

(defn trx
  "The translation of English UI string `s` in the disambiguating
  `context` under `ui`, or `s` itself.

  gettext's answer to one English word that several languages split.
  The context is part of the msgid, `context|string`, because the PO
  reader has no msgctxt; it never reaches the page, the fallback being
  `s` alone.

  (trx (->ui \"da\") \"button\" \"Search\")
  ;; => \"Søg\"   (the button; the page heading is \"Søgning\")"
  [ui context s]
  (get (:table ui) (str context "|" s) s))

(defn trn
  "The translation of English singular `s1` or plural `s2` for the
  count `n` under `ui`.

  The two are one gettext entry, so which form a count takes is the
  translator's to state rather than the view's. The rule here is the
  one Danish and English share: one is singular, everything else is
  plural. A language that divides them otherwise needs its rule added,
  and a PO reader that keeps more than two forms.

  The count may stand in the string itself, as `{n}`, filled from
  `values` when given (see `fill`).

  (trn (->ui \"da\") \"region\" \"regions\" 2)
  ;; => \"regioner\""
  ([ui s1 s2 n]
   (let [[one many] (get (:table ui) [s1 s2] [s1 s2])]
     (if (= 1 n) one many)))
  ([ui s1 s2 n values]
   (fill (trn ui s1 s2 n) values)))

(def number-formats
  "How each language writes a number: its thousands and decimal
  separators, which Danish and English swap.

  Not translations. A msgid of `.` says nothing to a translator, and
  the two languages would give the PO file two entries that differ in
  nothing a human could read."
  {"da" {:group "." :decimal ","}
   "en" {:group "," :decimal "."}})

(defn fixed
  "Number `n` written with `decimals` digits after the point, whatever
  the platform: a JVM double prints its .0 and a JavaScript number does
  not, so a rate of a whole number would read as a count in the
  browser and as a rate on the server."
  [n decimals]
  #?(:clj  (.toPlainString (.setScale (bigdec n) (int decimals)
                                      java.math.RoundingMode/HALF_UP))
     :cljs (.toFixed n decimals)))

(defn group-digits
  "Write number `n` the way `ui`'s language writes it: its digits
  grouped in thousands, its fraction (when it has one) after the
  decimal separator, and with exactly `decimals` digits of it when
  given (see `fixed`); nil for nil, so a statistic that could not be
  computed shows as nothing.

  Danish and English swap the two separators, so a rate beside a
  grouped count is ambiguous unless both follow the same language.

  (group-digits (->ui \"da\") 64600000)
  ;; => \"64.600.000\"

  (group-digits (->ui \"da\") 1234.5)
  ;; => \"1.234,5\"

  (group-digits (->ui \"en\") 1234 1)
  ;; => \"1,234.0\""
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
