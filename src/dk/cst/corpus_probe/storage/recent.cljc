(ns dk.cst.corpus-probe.storage.recent
  "The searches a reader has made lately: which params name the question
  one asked, what is kept of the answer it got, and how the list of them
  is written to the one value the browser stores.

  The mirror of dk.cst.corpus-probe.storage.settings, written as that
  one is and holding what it leaves out: the settings say how a reader
  reads an answer, and these say what they asked."
  (:require #?(:clj  [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.url :as url]))

(def store-key
  "The name the history is stored under in the browser's own store.

  Not a cookie: nothing the server answers needs it, a list of searches
  is longer than a cookie sent with every request should be, and one
  pasted list of words is longer than a cookie may be at all."
  "recent")

(def max-entries
  "How many searches the history holds. Enough that a morning's work is
  in it, few enough that the rail stays shorter than the page it stands
  beside."
  10)

(def narrowings
  "The params a reader narrows an answer by, working from beside it: a
  random sample of the hits, a word they must have nearby, and the
  subset behind a frequency row.

  They name which hits a URL holds, so
  dk.cst.corpus-probe.url/search-params keeps them; they narrow an
  answer to a question the history holds already, so this drops them,
  and working a control beside a result writes no entry of its own."
  [:sample :near :subset])

(def qualifiers
  "The params saying how a `narrowings` param narrows, which narrow
  nothing on their own."
  [:distance :subset-at :subset-attr])

(defn asked
  "The params of search `params` that name the question it asked: what
  identifies a search (see dk.cst.corpus-probe.url/search-params) less
  the `narrowings` of its answer and their `qualifiers`."
  [params]
  (apply dissoc (url/search-params params) (concat narrowings qualifiers)))

(defn refined?
  "True when search `params` narrow an answer rather than asking the
  question whole (see `narrowings`). What such a search counts is a part
  of what the question found, so no count is taken from it."
  [params]
  (boolean (some #(not-empty (str (get params %))) narrowings)))

(defn entry
  "What the history keeps of the search `params` asked: the canonical
  query string of the question (see `asked`), and from `facts`, what the
  answer reported, the `:hits` it found and the `:filter` it was
  narrowed by, in words. Nil for params that ask nothing, which is no
  search.

  The string is the entry's identity as well as its link: the same
  question asked again writes the same string (see `remember`)."
  [params facts]
  (when (query/of params)
    (into {:params (url/query-string (asked params))}
          (filter (comp some? val))
          (select-keys facts [:hits :filter]))))

(defn href
  "Where `entry` leads: the result of the search it remembers.

  Built from the string as it was written rather than read back into
  params and written again, since a metadata filter names one param per
  value and reading a repeated param keeps its last value alone (see
  dk.cst.corpus-probe.url/form-decode)."
  [{:keys [params]}]
  (str url/search "?" params url/results-fragment))

(defn string
  "The history `entries` as the one value they are stored under."
  [entries]
  (pr-str (vec entries)))

(defn remember
  "The history `entries` with `entry` at its head: the same question
  asked again moves rather than repeats, and the oldest go once there
  are more than `max-entries`.

  A question asked again keeps what it said of itself where it says
  nothing now: a narrowed answer counts a part of what the question
  found and reports no count of its own (see `refined?`)."
  [entries entry]
  (let [same? #(= (:params entry) (:params %))]
    (into [(merge (first (filter same? entries)) entry)]
          (comp (remove same?) (take (dec max-entries)))
          entries)))

(defn entry?
  "True when `x` is an entry as one was written: a question, and what its
  answer reported where it reported anything."
  [x]
  (boolean (and (map? x) (string? (:params x)) (not-empty (:params x))
                (or (nil? (:hits x)) (nat-int? (:hits x)))
                (or (nil? (:filter x)) (string? (:filter x))))))

(defn entries
  "The history the stored value `s` holds, kept to what was written as an
  entry (see `entry?`): none for a reader who has stored nothing, and
  none of what another version of this wrote, which is left behind
  rather than read."
  [s]
  (into []
        (filter entry?)
        (try
          (let [read (edn/read-string (str s))]
            (when (vector? read) read))
          (catch #?(:clj Exception :cljs :default) _ nil))))

(comment
  (entry {:q "hund" :corpus ["PROBE" "VISER"] :sort "word" :sample "50"}
         {:hits 3412 :filter "text_year 1591"})
  ;; => {:params "q=hund&corpus=PROBE,VISER", :hits 3412, :filter "text_year 1591"}

  (href {:params "q=hund&corpus=PROBE"})
  ;; => "/search?q=hund&corpus=PROBE#results"

  (entries (string (remember [] (entry {:q "hund"} {:hits 12}))))
  ;; => [{:params "q=hund", :hits 12}]

  #_.)
