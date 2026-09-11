(ns dk.cst.corpus-probe.storage.settings
  "The settings a reader stores as their own defaults: which of a
  search's params they are, and how they are written into the one value
  a cookie holds.

  Written as a query string but not a URL: the same params under a
  different rule, since a form is not a search."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.url :as url]))

(def param-keys
  "The search params a reader may store.

  Not the query itself, nor the metadata filter or the narrowings of a
  result, nor which view a result is shown in: those belong to one
  search rather than to the way a reader works."
  ;; TODO: several named sets, one per project a reader works on, each
  ;; holding these settings and no query. Several do not belong in a
  ;; cookie sent with every request, so they want a store of their own.
  [:corpus :scope :mode :in :ci :match :within
   :sort :context :attr :at :by :docs])

(def defaults
  "What a setting left out means: the defaults a URL applies, plus the
  form of the query, which no URL carries."
  (assoc url/defaults :mode (first mode/forms)))

(def cookie-key
  "The name the settings are stored under, which the form's buttons
  carry."
  :settings)

(def autosave-key
  "The key recording that a reader turned off automatic storing. It says
  how the settings are kept rather than what the form holds, so `params`
  leaves it out and it seeds no form."
  :autosave)

(def autosave-off
  "The `autosave-key` value turning automatic storing off. Storing is
  what a reader gets without asking, so only turning it off is stored."
  "off")

(defn autosave?
  "True when the stored settings `s` leave automatic storing on, which
  not mentioning it does (see `autosave-off`)."
  [s]
  (not= autosave-off (get (url/form-decode s) autosave-key)))

(def max-length
  "The longest the stored settings may be. A browser keeps about four
  kilobytes of a cookie and drops the rest without a word, so a selection
  larger than this is not stored at all."
  3000)

(defn storable?
  "True when `s` is short enough to store (see `max-length`). Which keys
  it names is `params`' business, not this one. Blank is storable: that
  is how the settings are forgotten."
  [s]
  (<= (count (str s)) max-length))

(defn string
  "The `param-keys` of `params` that depart from `defaults`, as the one
  value they are stored under, against the set `all` of every corpus the
  reader may choose; blank for a form at the app's own defaults.

  Every corpus chosen is kept as `url/all-scope` rather than by name,
  since a registry of many would not fit in a cookie. `url/form-encode`d
  rather than `url/query-string`, whose cosmetic commas a cookie value
  may not hold."
  ([params]
   (string params nil))
  ([params all]
   (let [chosen (url/corpora-param (:corpus params))
         all?   (and (seq all) (= (set chosen) (set all)))
         params (cond-> (url/with-corpora params all)
                  all? (assoc :scope url/all-scope))]
     (url/form-encode (for [k (conj param-keys autosave-key)
                            :let [v (str (get params k))]
                            :when (and (not (str/blank? v))
                                       (not= v (defaults k)))]
                        [(name k) v])))))

(defn params
  "The stored settings `s` as the params they seed a form with, kept to
  the `param-keys` so that nothing stored under another name is read
  back."
  [s]
  (select-keys (url/form-decode s) param-keys))

(def form-id
  "The id of the form storing them, so that its buttons can stand among
  the settings they store: a form cannot hold a form."
  "settings-form")

(def box-id
  "The id of the preferences box, which storing leaves the reader on: the
  button they pressed goes quiet as they press it, and a quiet button
  holds no focus."
  "preferences")

(defn with-autosave
  "The preferences `params` with their settings recording whether a
  change stores them: an unticked checkbox submits nothing, so nothing
  submitted is what turns it off.

  For the reader without a script, whose checkbox travels beside the
  button rather than inside its value. Settings stored as nothing are
  left alone: that is a reset, which puts automatic storing back too."
  [params]
  (let [stored (get params cookie-key)]
    (if (str/blank? (str stored))
      params
      (assoc params cookie-key
             (-> (url/form-decode stored)
                 (cond-> (not (contains? params autosave-key))
                   (assoc autosave-key autosave-off))
                 (string))))))

(defn reset?
  "True when `params` carry the settings and carry them empty, which is
  what the Reset button posts. The key has to be there: a post carrying
  only the language would read as blank too."
  [params]
  (and (contains? params cookie-key)
       (str/blank? (str (get params cookie-key)))))

(defn return
  "Where the preferences `params` send the reader: the `:return` they
  carry, or the bare form for a `reset?`, which is the only place the
  reset is visible."
  [params]
  (if (reset? params) url/search (:return params)))
