(ns dk.cst.corpus-probe.client.router
  "Where the client is and what it takes over: the location readers, the
  rule for which links and submits stay inside the app, and the document
  listeners, which only dispatch.

  Every route the server renders it also serves as transit, from the same
  handler, so the two can never describe different pages. A link click or
  a GET submit is fetched as data (see
  dk.cst.corpus-probe.client.effects/navigate!), rendered through the same
  .cljc views, and pushed onto the history; anything that fails falls
  back to a real navigation, which works because the server still renders
  every page in full."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.url :as url]))

(defonce shown
  ;; the path and query of the page on screen, so that a popstate leaving
  ;; them as they are, which is a jump to a fragment, is not taken for a
  ;; page to fetch again
  (atom nil))

(defn current-url
  "The current location as a mutable URL object."
  []
  (js/URL. js/location.href))

(defn params-of
  "The URLSearchParams `search-params` as the map the server reads a
  request's query params into: a param that repeats is a vector, the
  rest strings."
  [search-params]
  (let [m (atom {})]
    (.forEach search-params
              (fn [v k]
                (swap! m update (keyword k)
                       (fn [had]
                         (cond
                           (nil? had)    v
                           (vector? had) (conj had v)
                           :else         [had v])))))
    @m))

(defn url-params
  "The query params of `url`, a URL object (see `params-of`)."
  [url]
  (params-of (.-searchParams url)))

(defn form-params
  "The fields of `form` as a submit would send them, as the params map
  the server reads them into (see `params-of`)."
  [form]
  (params-of (js/URLSearchParams. (js/FormData. form))))

(defn location-params
  "The query params of the current location (see `params-of`)."
  []
  (url-params (current-url)))

(defn page-key
  "What names the page the location is on: its path and query, which a
  fragment is a place within."
  []
  (str js/location.pathname js/location.search))

(defn shown!
  "Record the page the location names as the one on screen (see `shown`)."
  []
  (reset! shown (page-key)))

(defn fragment
  "The place in the page `url`, a URL object, names: its fragment without
  the mark; nil when it names none."
  [url]
  (let [hash (.-hash url)]
    (when (seq hash)
      (js/decodeURIComponent (subs hash 1)))))

(defn cited-href
  "Absolute `href` as the history should hold it: without the results
  fragment, which tells a browser where to land and a reader nothing, and
  which the client lands without (see
  dk.cst.corpus-probe.client.effects/land!). Any other fragment is a
  place in the page and stays."
  [href]
  (let [url (js/URL. href)]
    (when (= url/results-fragment (.-hash url))
      (set! (.-hash url) ""))
    (.-href url)))

(defn landed-href
  "The address a routed navigation to absolute `href` lands on when its
  answer came from `landed`: that address, which is elsewhere after a
  redirect, with the fragment of `href`, which a fetch never sends and a
  browser keeps across a redirect. `href` itself when the answer names
  no address."
  [href landed]
  (if (seq landed)
    (let [url (js/URL. landed)]
      (set! (.-hash url) (.-hash (js/URL. href)))
      (.-href url))
    href))

(def routable-paths
  "The paths the client renders itself. Anything else stays the browser's
  business: an export is a download rather than a page, and fetching one
  as data would run the search a second time and then hand the reader
  nothing."
  #{url/home url/search url/corpora url/glossary url/cqp-guide})

(defn routable?
  "True when `url` names a page this client knows how to render."
  [url]
  (let [path (.-pathname url)]
    (or (contains? routable-paths path)
        (str/starts-with? path (str url/corpora "/")))))

(defn in-page?
  "True when `url` names a place in the page the reader is on: an anchor
  the browser should follow itself, as it does the bypass link, rather
  than the page being fetched again to arrive where it already is."
  [url]
  (and (seq (.-hash url))
       (= (.-pathname url) js/location.pathname)
       (= (.-search url) js/location.search)))

(defn routed?
  "True when `el` is a link this app renders itself, clicked by `event`:
  a page it knows, same origin, not a place in this one, no target and no
  modifier, so the browser's own meaning of the click is kept."
  [el event]
  (and el
       (= (.-origin (js/URL. (.-href el))) js/location.origin)
       (routable? (js/URL. (.-href el)))
       (not (in-page? (js/URL. (.-href el))))
       (str/blank? (.-target el))
       (not (or (.-metaKey event) (.-ctrlKey event)
                (.-shiftKey event) (.-altKey event)))
       (not= 1 (.-button event))))

(defn selectable-corpora
  "The IDs of every corpus `form` lets the reader choose: its corpus
  boxes that are not disabled, which is what the chooser offers and what
  a URL naming no corpus searches."
  [form]
  (into #{}
        (map #(.-value %))
        (.querySelectorAll form "input[name=corpus]:not(:disabled)")))

(defn form-query
  "The query string of a submit of `form`, as the URL cites it (see
  dk.cst.corpus-probe.url/canonical): the browser's own rules for what a
  form submits, less empty fields and defaults, which say nothing."
  [form]
  (url/query-string (url/canonical (form-params form)
                                   (selectable-corpora form))))

(defn submit-href
  "The address a GET submit of `form` asks for: its action with the query
  string of its fields (see `form-query`)."
  [form]
  (let [url (js/URL. (.-action form))]
    (set! (.-search url) (form-query form))
    (.-href url)))

(defn routed-submit?
  "True when a submit of `form` is one this app answers itself: a GET to
  its own origin."
  [form]
  (and (= "get" (str/lower-case (or (.-method form) "get")))
       (= (.-origin (js/URL. (.-action form))) js/location.origin)))

(defn listen!
  "Install the document's listeners, which only dispatch through
  `dispatch!`: a click on a link this app renders itself (see `routed?`)
  and a submit to its own origin are `[:navigate href true]`, the
  language switch's submit `[:set-preference k v]`, a popstate to
  another page `[:navigate href false]`, a hashchange `[:set-fragment
  fragment]`, and a press outside the fieldset of any of the lists `ks`
  `[:leave k true]` (see dk.cst.corpus-probe.client.lists/leave).

  A fragment navigation fires popstate too, and the page it is within is
  already on screen (see `shown`): the browser has moved to the place
  itself, as it does for the bypass link, and fetching the page again
  would only take the reader somewhere else.

  The press outside a list is the one gesture no element of its fieldset
  can hear, and the one a reader makes to go on to something else. A
  press rather than a click, so that a drag begun elsewhere counts, and a
  press rather than focus, since a label, a summary's words and the page
  around them take no focus, and on Safari neither does a box. The
  fieldset is named for its list in a data attribute (see
  dk.cst.corpus-probe.views.chooser/fieldset)."
  [dispatch! ks]
  (.addEventListener
   js/document "click"
   (fn [e]
     (when-not (.-defaultPrevented e)
       (when-let [link (some-> (.-target e) (.closest "a[href]"))]
         (when (routed? link e)
           (.preventDefault e)
           (dispatch! [:navigate (.-href link) true]))))))
  (.addEventListener
   js/document "submit"
   (fn [e]
     (let [form (.-target e)]
       (cond
         (.-defaultPrevented e) nil

         ;; the language switch changes a preference, not a place: store
         ;; it and ask the server for this same page in the other
         ;; language, rather than posting and reloading
         (= url/preferences (.getAttribute form "action"))
         (let [b (.-submitter e)]
           (.preventDefault e)
           (dispatch! [:set-preference (.-name b) (.-value b)]))

         (routed-submit? form)
         (do (.preventDefault e)
             (dispatch! [:navigate (submit-href form) true]))))))
  (.addEventListener js/window "popstate"
                     (fn [_]
                       (when (not= (page-key) @shown)
                         (dispatch! [:navigate js/location.href false]))))
  ;; a link within the page, or back and forth between two places in
  ;; it, changes only what the page marks
  (.addEventListener js/window "hashchange"
                     (fn [_]
                       (dispatch! [:set-fragment (fragment (current-url))])))
  (.addEventListener
   js/document "pointerdown"
   (fn [e]
     (doseq [k ks]
       (when-not (some-> (.-target e)
                         (.closest (str "[data-list=" (name k) "]")))
         (dispatch! [:leave k true]))))))
