(ns dk.cst.corpus-probe.client.effects
  "The edge of the client: everything that touches the world, the timers,
  the fetches, focus, the history, the document's title and language,
  the cookie, and `perform!`, which runs the effects an action answered
  with. Nothing here reads or writes the state: a fetch comes back as an
  action, so what to keep of a late answer is the pure step's decision."
  (:require [cognitect.transit :as transit]
            [dk.cst.corpus-probe.client.router :as router]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.concordance :as concordance]))

(defonce ^{:doc "The wait before a routed navigation is worth reporting,
  so that an answer arriving at once is not announced and then
  unannounced."}
  pending-timer
  (atom nil))

(defonce ^{:doc "The pending debounce of a metadata filter refresh, so
  that a reader ticking their way through a folder asks for one set of
  filters at the end rather than one per box."}
  filters-timer
  (atom nil))

(defonce ^{:doc "The AbortController of the routed navigation being
  fetched, if there is one, so that starting another can call off the one
  it replaces."}
  in-flight
  (atom nil))

(def pending-delay-ms
  "How long a routed navigation may take before it is worth saying that
  it is in flight."
  ;; a dev search answers at once and a whole-corpus sort runs for
  ;; minutes: nothing is said until a reader has begun to wonder
  400)

(def filters-debounce-ms
  "How long the corpus selection must hold still before the metadata
  filters it offers are fetched: long enough that ticking several boxes
  is one request, short enough that a reader who has stopped is not left
  waiting."
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
  do: for a control that fires while a reader is still working it, only
  the state they stop on is worth acting on."
  [timer ms f]
  (cancel! timer)
  (reset! timer (js/setTimeout f ms)))

(defn fetch-transit!
  "Fetch `href` as transit, called off by `signal` where there is one: a
  promise of `[data landed]`, the decoded answer and the address it came
  from, rejected when the request fails."
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
  key."
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
  still being counted and dispatch its arrival through `dispatch!`:
  `[:counts-arrived counted]`."
  [dispatch! state]
  (when (seq (get-in state [:result :remaining]))
    (let [key (router/page-key)]
      ;; the page's own query string, so the server counts the question
      ;; the page answered; dropped once the reader has moved on
      (-> (fetch-transit! (str url/counts-api js/location.search))
          (.then (fn [[counted]]
                   (when (= key @router/shown)
                     (dispatch! [:counts-arrived counted]))))
          ;; a real navigation, as a page that fails: the server renders
          ;; the page with its count in full
          (.catch (fn [_]
                    (when (= key @router/shown)
                      (set! (.-href js/location) js/location.href))))))))

(defn fetch-expansions!
  "Fetch the wider context of every hit `state` holds as loading in
  `:expanded`, each arriving through `dispatch!` (see `fetch-context!`)."
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
  `[:filters-due]` through `dispatch!`."
  [dispatch!]
  (debounce! filters-timer filters-debounce-ms #(dispatch! [:filters-due])))

(defn navigate!
  "Fetch the route at absolute `href` as data and dispatch its arrival
  through `dispatch!`: `[:page-arrived data landed push?]`, `landed`
  being where the answer came from and `push?` whether it gets a history
  entry; `[:pending]` once an answer is `pending-delay-ms` late. Falls
  back to a real navigation on any failure, so a route the client cannot
  render is still a working page."
  [dispatch! href push?]
  (let [controller (js/AbortController.)]
    ;; called off rather than raced, so the reader gets the answer to
    ;; their latest question
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
                  ;; an abort is no failure to fall back from, and its
                  ;; timer belongs to the navigation that did the
                  ;; aborting, still in flight and maybe worth reporting
                  (when-not (.-aborted (.-signal controller))
                    (cancel! pending-timer)
                    (set! (.-href js/location) href)))))))

(defn set-cookie!
  "Store `v` under setting `k` in the cookie the server reads (see
  dk.cst.corpus-probe.url/cookie), so a reload and every later visit
  carry the setting too."
  [k v]
  (set! (.-cookie js/document) (url/cookie k v)))

(defn set-preference!
  "Store `v` under setting `k` (see `set-cookie!`) and fetch this page
  again with it applied, dispatching through `dispatch!`: a fetch rather
  than a re-render, since the server words the document title and the
  result summaries."
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
  it there has run: where a reader who added or took away a token or a
  condition is left, rather than on the body."
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
  routed change would otherwise leave the page saying it is in the
  language it was served in."
  [lang]
  (set! (.-lang (.-documentElement js/document)) lang))

(defn resubmit!
  "Submit the form with `form-id` again, as it now stands, at once: a
  <select> reports only the value a reader settled on. Through the form
  rather than a built URL, so that the sort or the grouping travels with
  everything else the form holds, by the routed path a press would take."
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
  chosen; both are written to the element on every render."
  [node {:keys [indeterminate invalid]}]
  (set! (.-indeterminate node) indeterminate)
  (.setCustomValidity node (or invalid "")))

(defn leave-concordance!
  "Close the inspection panel, `[:inspect nil]` through `dispatch!`, once
  focus has settled outside both the concordance and the panel, which
  are one pool: focus moving between them keeps the panel."
  [dispatch!]
  ;; a tick later, since focusout fires before the next element has
  ;; focus; and activeElement rather than relatedTarget, so that a click
  ;; on the page background closes the panel while switching windows,
  ;; which keeps the active element, does not
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
  it means to be looking at the start of something already: a region
  beginning near the foot shows one row of itself."
  [el]
  (let [top (.-top (.getBoundingClientRect el))]
    (and (>= top 0) (< top (/ (.-innerHeight js/window) 2)))))

(defn land!
  "Put the reader where a routed navigation should leave them: at the
  place in the page the URL's fragment names, when it names one; else
  focused on the results, when the page has any, and moved to them only
  if they are not already at hand; else at the top of the page."
  []
  (let [hash   (.-hash js/location)
        target (.getElementById js/document url/results-id)]
    (cond
      ;; replacing the location with itself is a fragment navigation,
      ;; which scrolls, marks the :target and sets where Tab starts, none
      ;; of which scrollIntoView does; its popstate names the page on
      ;; screen, so the router ignores it
      (and (seq hash) (not= hash url/results-fragment))
      (.replace js/location js/location.href)

      ;; focus is what tells a reader the outcome arrived; the browser
      ;; scrolls on a real navigation, from the fragment on the form
      ;; action, but pushState does not
      target
      (do (when-not (at-hand? target)
            (.scrollIntoView target))
          (.focus target #js {:preventScroll true}))

      :else
      (.scrollTo js/window 0 0))))

(defn sync-url!
  "Mirror the hits `state` shows expanded in the URL's `expand` parameter,
  replacing history so the URL stays shareable without new entries, and
  record the page on screen (see dk.cst.corpus-probe.client.router/shown!)."
  [state]
  ;; only on the search page, whose query string the rule knows (the
  ;; reading page names its position in one), and not on a switch URL,
  ;; whose query of the old form the rule would drop, leaving a reload
  ;; an empty form (see dk.cst.corpus-probe.query/arrived)
  (when (and (= url/search js/location.pathname)
             (not (mode/unread-query? (router/location-params))))
    (let [url    (router/current-url)
          params (url/with-expanded (router/location-params)
                                    (keys (:expanded state)))]
      ;; the whole query string, so the bar shows the canonical URL
      ;; whatever was typed
      (set! (.-search url) (url/query-string params))
      ;; only when it would say something new: Safari throws past a
      ;; hundred history writes in thirty seconds
      (when (not= (.-href url) js/location.href)
        (replace-url! (.-href url)))))
  (router/shown!))

(defn perform!
  "Run `effects`, the vectors an action answered with, in order, for the
  Replicant dispatch `data` of the event or hook that raised it, with
  the `:state` the step left under it; what comes back from the world
  goes through `dispatch!` as an action."
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
