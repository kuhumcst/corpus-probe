(ns dk.cst.corpus-probe.query.mode
  "The two forms of the query and the modes they are read in: which form
  a set of search params belongs to (`form`), which mode reads them
  (`mode`), what each mode reads of them (`fields` and `reads?`) and
  what it leaves unread (`unread`)."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.query.tokens :as tokens]))

(def modes
  "The query modes, each a way the query params are read: words in order,
  a list of words, the tokens of the extended form, and CQP as the reader
  wrote it. The first is the default; the three that are text are read
  from one field by the shape of what it holds (see `shape`)."
  ["simple" "list" "extended" "cqp"])

(def forms
  "The two forms of the query, in display order, each as the value of
  the form's `mode` radio: the field, named for the mode it starts in,
  and the extended form of tokens."
  ["simple" "extended"])

(def cqp-start
  "What CQP text begins with, once trimmed: a pattern in brackets or a
  quoted form, a tag, a group, a target mark or a meet-union or table
  query. A bare word is none of these, since CQP reads one as the name
  of a query result, which is how the field tells CQP from words."
  #"^\s*(?:[\[\"'<(@]|MU\s*\(|TAB\s*\()")

(defn shape
  "The mode the `text` of the query field is read in: CQP when it begins
  as CQP does (see `cqp-start`), a list when it holds a line break, and
  words in order otherwise, a blank included."
  [text]
  (let [text (str text)]
    (cond
      (str/blank? text)        "simple"
      (re-find cqp-start text) "cqp"
      (re-find #"[\r\n]" text) "list"
      :else                    "simple")))

(def fields
  "What each of the `modes` reads of the query params: the keys that say
  what was asked, by the mode that reads them; `::tokens` stands for the
  fields of an extended search's tokens. A param outside its mode's set
  says nothing to the search, so a URL does not carry it and the form
  does not show its control."
  {"simple"   #{:q :in :ci :match :within}
   "list"     #{:q :in :ci :match}
   "extended" #{::tokens :within}
   "cqp"      #{:q}})

(def defaults
  "What a query key means when a URL leaves it out, as the string it
  would carry. `match` and `ci` have no value to leave out: the whole
  form, as written, is what their absence says."
  {:in     "word"
   :within "sentence"})

(defn typed
  "The mode the query of `params` was typed in (see `modes`): the
  extended form's when they carry the field of a token, else the shape
  of the field's text, `q`; nil when they carry neither."
  [params]
  ;; tokens first, so that a URL carrying both is read as tokens and told
  ;; of the text
  (cond
    (some tokens/token-key? (keys params)) "extended"
    (contains? params :q)                  (shape (:q params))))

(defn form-of
  "The form (see `forms`) query `mode` is read from: the extended form
  for its tokens, the field for the rest, nil included."
  [mode]
  (if (= "extended" mode) "extended" "simple"))

(defn form
  "The form of the search `params` (see `forms`): the one their `mode`
  param names, when it is a form, which is what a submitted form's radio
  says; else the form of the mode their query was `typed` in."
  [params]
  (let [m (:mode params)]
    (if (some #{m} forms) m (form-of (typed params)))))

(defn mode
  "The mode the search `params` are read in (see `modes`): the extended
  form's tokens when that is their `form`, else the shape of the field's
  text (see `shape`), words in order when there is none."
  [params]
  (if (= "extended" (form params))
    "extended"
    (shape (:q params))))

(defn query-key?
  "True when param key `k` says what was asked: the mode, a key some mode
  reads (see `fields`) or the field of a token."
  [k]
  (boolean (and k (not= ::tokens k)
                (or (= :mode k)
                    (some #(contains? % k) (vals fields))
                    (tokens/token-key? k)))))

(defn reads?
  "True when mode `m` reads param key `k` (see `fields`): the mode itself,
  one of the mode's keys or, where it reads tokens, the field of one.
  The marker standing for the tokens is no key of its own."
  [m k]
  (let [own (get fields m)]
    (boolean (and (not= ::tokens k)
                  (or (= :mode k)
                      (contains? own k)
                      (and (contains? own ::tokens) (tokens/token-key? k)))))))

(defn read-keys
  "The query params among `params` that the form of mode `m` reads (see
  `reads?`), as keys, the mode itself aside: what a form holds of a
  query, and so what its query replaces when it changes."
  [m params]
  (filter #(and (query-key? %) (not= :mode %) (reads? m %))
          (keys params)))

(defn unread
  "The keys of the query params among `params` that their mode, or the
  mode `m` given, does not read: what another mode's form or a
  hand-written URL carried along, which the search never sees."
  ([params]
   (unread params (mode params)))
  ([params m]
   (into #{}
         (filter #(and (query-key? %) (not (reads? m %))))
         (keys params))))

(defn without-unread
  "Drop from `params` what the mode `m` does not read (see `unread`)."
  [params m]
  (apply dissoc params (unread params m)))

(defn unread-query?
  "True when `params` carry a query their mode does not read (see
  `unread`) that says something: the field's text under the extended
  form, or a token that asks under the field's; a blank field or the
  blank trailing token every form submits is no query."
  [params]
  (let [unread (unread params)]
    (boolean (or (and (unread :q) (tokens/present (:q params)))
                 (and (some tokens/token-key? unread)
                      (some tokens/asks? (tokens/token-rows params)))))))

(comment
  (map shape ["hund" "lille hund" "hund\nkat" "[lemma = \"hund\"]" "\"hund\""])
  ;; => ("simple" "simple" "list" "cqp" "cqp")

  (mode {:mode "extended" :q "hund"})
  ;; => "extended"
  #_.)
