(ns dk.cst.corpus-probe.client.effects
  "The edge of the client: everything that touches the world, the timers,
  the fetches, focus, the history, the document's title and language, the
  cookie, and `perform!`, which runs the effects an action answered with
  (see dk.cst.corpus-probe.client.actions/act).

  Nothing here reads or writes the state. A fetch comes back as an
  action: what arrived is dispatched, and the pure step decides what to
  keep of it, so a late answer to a question the reader has moved on from
  is a decision of the step rather than of a callback."
  (:require [cognitect.transit :as transit]
            [dk.cst.corpus-probe.client.router :as router]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.concordance :as concordance]))

(defonce pending-timer
  ;; the wait before a routed navigation is worth reporting, so that an
  ;; answer arriving at once is not announced and then unannounced
  (atom nil))

(defonce filters-timer
  ;; the pending debounce of a metadata filter refresh, so that a reader
  ;; ticking their way through a folder asks for one set of filters at the
  ;; end rather than one per box
  (atom nil))

(defonce in-flight
  ;; the AbortController of the routed navigation being fetched, if there
  ;; is one, so that starting another can call off the one it replaces
  (atom nil))

(def pending-delay-ms
  "How long a routed navigation may take before it is worth saying that
  it is in flight.

  A search of the dev registry answers in tens of milliseconds, and
  saying so and then unsaying it is a flicker where the reader asked a
  question: worse than saying nothing. The case this exists for is the KU
  registry, where a whole-corpus sort has been measured at 649 seconds
  against a 300 second timeout. So nothing is said until an answer is
  late enough that a reader has begun to wonder."
  400)

(def filters-debounce-ms
  "How long the corpus selection must hold still before the metadata
  filters it offers are fetched. Long enough that ticking several boxes in
  a row is one request, short enough that a reader who has stopped is not
  left waiting on a timer."
  300)

(defn read-transit
  "Decode transit-JSON string `s`."
  [s]
  (transit/read (transit/reader :json) s))

(defn cancel!
  "Call off whatever `timer` was waiting to do."
  [timer]
  (some-> @timer js/clearTimeout)
  (reset! timer nil))

(defn debounce!
  "Call `f` after `ms`, cancelling whatever `timer` was already waiting to
  do.

  For a control that fires while a reader is still working it: only the
  state they stop on is worth acting on, and the ones on the way are
  worth nothing and cost a search each."
  [timer ms f]
  (cancel! timer)
  (reset! timer (js/setTimeout f ms)))

(defn fetch-transit!
  "Fetch `href` as transit, called off by `signal` where there is one: a
  promise of `[data landed]`, the decoded answer and the address it came
  from (see dk.cst.corpus-probe.client.router/landed-href), rejected when
  the request fails."
  ([href]
   (fetch-transit! href nil))
  ([href signal]
   (-> (js/fetch href #js {:headers #js {"Accept" url/transit-type}
                           :signal  signal})
       (.then (fn [response]
                (if (.-ok response)
                  (.then (.text response)
                         (fn [body] [(read-transit body) (.-url response)]))
                  (throw (js/Error. (str "request failed: " href)))))))))

(defn fetch-context!
  "Fetch the hit at `cpos`/`matchend` in `corpus` with wider context and
  dispatch its arrival through `dispatch!`: `[:context-arrived k hit]`,
  or `[:context-failed k]` when the request fails, `k` being the hit's
  key (see dk.cst.corpus-probe.views.concordance/hit-key)."
  [dispatch! corpus cpos matchend]
  (let [k [corpus cpos]]
    (-> (fetch-transit! (str url/context-api "?corpus=" corpus
                             "&cpos=" cpos "&matchend=" matchend))
        (.then (fn [[hit]] (dispatch! [:context-arrived k hit])))
        (.catch (fn [_] (dispatch! [:context-failed k]))))))

(defn fetch-filters!
  "Fetch the metadata filters `corpora` offer and dispatch their arrival
  through `dispatch!`: `[:filters-arrived corpora options]`, or
  `[:filters-failed]` when the request fails."
  [dispatch! corpora]
  (let [params (js/URLSearchParams.)]
    (doseq [corpus corpora] (.append params "corpus" corpus))
    (-> (fetch-transit! (str url/filters-api "?" params))
        (.then (fn [[options]]
                 (dispatch! [:filters-arrived corpora options])))
        (.catch (fn [_] (dispatch! [:filters-failed]))))))

(defn fetch-counts!
  "Fetch the count of the search `state` shows while its corpora are
  still being counted (see dk.cst.corpus-probe.search/concordance!) and
  dispatch its arrival through `dispatch!`: `[:counts-arrived counted]`.

  Asked with the page's own query string, so the server counts the
  question the page answered. Dispatched only while that page is still
  the one on screen (see dk.cst.corpus-probe.client.router/shown), so a
  count arriving after the reader has moved on is dropped rather than
  written over the answer to their next question. A count that fails
  falls back to a real navigation, as a page that fails does: the server
  renders the page with its count in full."
  [dispatch! state]
  (when (seq (get-in state [:result :remaining]))
    (let [key (router/page-key)]
      (-> (fetch-transit! (str url/counts-api js/location.search))
          (.then (fn [[counted]]
                   (when (= key @router/shown)
                     (dispatch! [:counts-arrived counted]))))
          (.catch (fn [_]
                    (when (= key @router/shown)
                      (set! (.-href js/location) js/location.href))))))))

(defn fetch-expansions!
  "Fetch the wider context of every hit `state` holds as loading in
  `:expanded` (see dk.cst.corpus-probe.views.concordance/loading), each
  arriving through `dispatch!` (see `fetch-context!`)."
  [dispatch! state]
  (doseq [[[corpus cpos :as k] v] (:expanded state)
          :when (= concordance/loading v)
          :let [hit (first (filter #(= k (concordance/hit-key %))
                                   (get-in state [:result :hits])))]
          :when hit]
    (fetch-context! dispatch! corpus cpos (:matchend (:anchors hit)))))

(defn refresh-filters!
  "Ask, once the corpus selection has held still for
  `filters-debounce-ms`, whether the metadata filters want fetching:
  `[:filters-due]` through `dispatch!`, which decides (see
  dk.cst.corpus-probe.client.lists/filters-stale?)."
  [dispatch!]
  (debounce! filters-timer filters-debounce-ms #(dispatch! [:filters-due])))

(defn navigate!
  "Fetch the route at absolute `href` as data and dispatch its arrival
  through `dispatch!`: `[:page-arrived data landed push?]`, `landed`
  being where the answer came from (see
  dk.cst.corpus-probe.client.router/landed-href) and `push?` whether it
  gets a history entry; a popstate replaces nothing.

  Falls back to a real navigation on any failure, so a route the client
  cannot render is still a working page: the server renders every one of
  them.

  Dispatches `[:pending]` once an answer is `pending-delay-ms` late, and
  not before. A whole-corpus search can run for minutes, and until the
  client routed anything the browser reported that wait itself; an answer
  that arrives at once wants no report at all.

  A navigation started while one is in flight calls the first off rather
  than racing it, so the reader gets the answer to their latest question,
  and the abandoned one does not mistake being called off for failing and
  load the page the reader has already left."
  [dispatch! href push?]
  (let [controller (js/AbortController.)]
    (some-> @in-flight (.abort))
    (reset! in-flight controller)
    (debounce! pending-timer pending-delay-ms #(dispatch! [:pending]))
    (-> (fetch-transit! href (.-signal controller))
        (.then (fn [[data landed]]
                 ;; before the state is replaced, or a report scheduled
                 ;; for a wait that is over lands on the answer to it
                 (cancel! pending-timer)
                 (dispatch! [:page-arrived data (router/landed-href href landed)
                             push?])))
        (.catch (fn [_]
                  ;; an abort leaves the timer alone: it belongs to the
                  ;; navigation that did the aborting, which is still in
                  ;; flight and may yet be worth reporting
                  (when-not (.-aborted (.-signal controller))
                    (cancel! pending-timer)
                    (set! (.-href js/location) href)))))))

(defn set-cookie!
  "Store `v` under setting `k` in the cookie the server reads, so a
  reload and every later visit carry the setting too."
  [k v]
  (set! (.-cookie js/document)
        (str k "=" v ";Path=/;Max-Age=31536000;SameSite=Lax")))

(defn set-preference!
  "Store `v` under setting `k` (see `set-cookie!`) and fetch this page
  again with it applied (see `navigate!`), dispatching through
  `dispatch!`.

  A fetch rather than a re-render, because the server words the document
  title and the result summaries, not the client. The server decides what
  a setting accepts, so nothing is validated here: a reader can only
  mislead themselves."
  [dispatch! k v]
  (set-cookie! k v)
  (navigate! dispatch! js/location.href false))

(defn focus!
  "Move focus to the element with `id`, once the render that put it there
  has run."
  [id]
  (some-> (.getElementById js/document id) (.focus)))

(defn focus-field!
  "Move focus to the form control named `name`, once the render that put
  it there has run: a reader who added or took away a token or a
  condition is left on what is now there rather than on the body, which
  is where focus falls when the button under it goes."
  [name]
  (some-> (.querySelector js/document (str "[name=\"" name "\"]"))
          (.focus)))

(defn push-url!
  "Add `href` to the history as the address of the page on screen."
  [href]
  (.pushState js/history nil "" href))

(defn replace-url!
  "Make `href` the address of the page on screen, without a history entry."
  [href]
  (.replaceState js/history nil "" href))

(defn set-title!
  "Give the document the title `s`."
  [s]
  (set! (.-title js/document) s))

(defn set-lang!
  "Give the document the language `lang`, which only the server sets: a
  routed change of it would otherwise leave the page saying it is in the
  language it was served in while every word on it is in another."
  [lang]
  (set! (.-lang (.-documentElement js/document)) lang))

(defn resubmit!
  "Submit the form with `form-id` again, as it now stands.

  At once, because a <select> reports the value a reader settled on
  rather than the ones they passed over on the way: choosing an order is
  asking for it, and any wait between the two is a wait nobody asked for.

  Through the form rather than by building a URL, so that the sort or the
  grouping travels with everything else the form holds and takes the same
  routed path a reader pressing the button would."
  [form-id]
  (some-> (.getElementById js/document form-id) (.requestSubmit)))

(defn prevent-default!
  "Keep the browser from answering `event` itself."
  [event]
  (.preventDefault event))

(defn set-validity!
  "Write `msg`, the constraint the markup cannot state, to control `node`:
  the message it reports, or nothing for nil."
  [node msg]
  (.setCustomValidity node (or msg "")))

(defn set-checkbox-state!
  "Write to checkbox `node` the states of `m` that no attribute carries:
  `:indeterminate`, for a folder holding only part of the selection, and
  `:invalid`, the message the corpus chooser reports while nothing is
  chosen. A checkbox has three states and only two of them are
  attributes, and the constraint of a group of them is no attribute of
  any one box, so both are written to the element itself on every
  render."
  [node {:keys [indeterminate invalid]}]
  (set! (.-indeterminate node) indeterminate)
  (.setCustomValidity node (or invalid "")))

(defn leave-concordance!
  "Close the inspection panel, `[:inspect nil]` through `dispatch!`, once
  focus has settled outside both the concordance and the panel, which are
  one pool: focus moving between them keeps the panel and focus leaving
  either for the page closes it, so both report focus leaving, and which
  of them did does not matter.

  Deferred by a tick because focusout fires before the next element has
  focus, and read from `activeElement` rather than the event's
  relatedTarget so that clicking the page background closes the panel
  while merely switching windows does not: a blurred window keeps its
  active element, an abandoned concordance does not."
  [dispatch!]
  (js/setTimeout
   (fn []
     (let [el     (.-activeElement js/document)
           region (.getElementById js/document concordance/region-id)
           panel  (.getElementById js/document concordance/inspector-id)]
       (when-not (or (and region (.contains region el))
                     (and panel (.contains panel el)))
         (dispatch! [:inspect nil]))))
   0))

(defn at-hand?
  "True when `el` begins in the upper half of the viewport, which is what
  it means to already be looking at the start of something.

  Anywhere on screen is too weak a test: a region beginning near the foot
  of the viewport shows one row of itself, and a reader who has just asked
  a question is owed more of the answer than that."
  [el]
  (let [top (.-top (.getBoundingClientRect el))]
    (and (>= top 0) (< top (/ (.-innerHeight js/window) 2)))))

(defn land!
  "Put the reader where a routed navigation should leave them: at the
  place in the page the URL's fragment names, when it names one; else
  focused on the results, when the page has any, and moved to them only
  if they are not already on screen; else at the top of the page.

  A fragment other than the results is handed to the browser, since
  replacing the location with itself is a fragment navigation, which
  scrolls, marks the `:target` and sets where Tab starts, none of which
  scrollIntoView does; the popstate it fires names the page on screen,
  so the router ignores it.

  Focus moves to the results because that is what tells a reader the
  outcome arrived. Scrolling to them only happens when they are not
  already at hand: switching the view of a result, or searching again
  beside one, leaves the page where it is, while a turn of the page from
  the foot of a long one, or a first search on a narrow screen, brings
  them up. The browser scrolls on a real navigation, from the fragment
  on the form action; pushState does not, so the client decides."
  []
  (let [hash   (.-hash js/location)
        target (.getElementById js/document url/results-id)]
    (cond
      (and (seq hash) (not= hash url/results-fragment))
      (.replace js/location js/location.href)

      target
      (do (when-not (at-hand? target)
            (.scrollIntoView target))
          (.focus target #js {:preventScroll true}))

      :else
      (.scrollTo js/window 0 0))))

(defn sync-url!
  "Mirror the hits `state` shows expanded in the URL's `expand` parameter
  as `CORPUS:cpos` items, replacing history so the URL stays shareable
  without new entries, and record the page on screen (see
  dk.cst.corpus-probe.client.router/shown!), so a fragment jump is not
  taken for a page to fetch.

  The whole query string is rewritten rather than one param set on it,
  so the URL in the bar is the canonical one whatever was typed:
  `mode=simple` goes and the corpora become one param. Only on the search
  page, whose query string is the one that rule knows: on any other
  page it would drop what it does not know, and the reading page names
  its position that way. And only when it would say something new:
  Safari throws past roughly a hundred history writes in thirty seconds."
  [state]
  ;; a switch URL, carrying the query of the mode the form was in, is
  ;; left as it is: the rule would drop that query, and a reload would
  ;; then find an empty form (see dk.cst.corpus-probe.query/arrived)
  (when (and (= url/search js/location.pathname)
             (not (mode/unread-query? (router/location-params))))
    (let [url    (router/current-url)
          params (url/with-expanded (router/location-params)
                                    (keys (:expanded state)))]
      (set! (.-search url) (url/query-string params))
      (when (not= (.-href url) js/location.href)
        (replace-url! (.-href url)))))
  (router/shown!))

(defn perform!
  "Run `effects`, the vectors an action answered with, in order, for the
  Replicant dispatch `data` of the event or hook that raised it, with the
  `:state` the step left under it; what comes back from the world goes
  through `dispatch!` as an action.

  `[:prevent-default]` and the two render hooks, `[:set-validity msg]`
  and `[:set-checkbox-state m]`, read the event or the node from `data`;
  `[:fetch-counts]`, `[:fetch-expansions]` and `[:sync-url]` read the
  state; the rest carry what they need."
  [dispatch! {:keys [state] :as data} effects]
  (doseq [[effect & args] effects]
    (case effect
      :prevent-default    (prevent-default! (:replicant/dom-event data))
      :focus              (apply focus! args)
      :focus-field        (apply focus-field! args)
      :fetch-context      (apply fetch-context! dispatch! args)
      :fetch-filters      (apply fetch-filters! dispatch! args)
      :fetch-counts       (fetch-counts! dispatch! state)
      :fetch-expansions   (fetch-expansions! dispatch! state)
      :refresh-filters    (refresh-filters! dispatch!)
      :navigate           (apply navigate! dispatch! args)
      :set-preference     (apply set-preference! dispatch! args)
      :push-url           (apply push-url! args)
      :set-title          (apply set-title! args)
      :set-lang           (apply set-lang! args)
      :resubmit           (apply resubmit! args)
      :set-validity       (apply set-validity! (:replicant/node data) args)
      :set-checkbox-state (apply set-checkbox-state! (:replicant/node data)
                                 args)
      :leave-concordance  (leave-concordance! dispatch!)
      :land               (land!)
      :sync-url           (sync-url! state))))
