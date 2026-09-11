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
            [dk.cst.corpus-probe.views.concordance :as concordance]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

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

(defonce ^{:doc "What the concordance was last centred on. This is the
  token, the reach, and the width of the strip that is read (refer to
  `centre-match!`). A render that changed none of these leaves the reader
  where they scrolled to."}
  centred
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

(defn fetch-wider!
  "Fetch the page on screen holding `reach` words either side of each
  match and dispatch its arrival through `dispatch!`: `[:wider-arrived
  reach data]`. The page's own query string with the reach added, so the
  server answers the question the page answered; the URL in the bar is
  left as it is, a reach being a way of reading a result rather than part
  of naming one. Dropped once the reader has moved on."
  [dispatch! reach]
  (let [key (router/page-key)
        url (router/current-url)]
    (.set (.-searchParams url) "reach" (str reach))
    (-> (fetch-transit! (str (.-pathname url) (.-search url)))
        (.then (fn [[data]]
                 (when (= key @router/shown)
                   (dispatch! [:wider-arrived reach data]))))
        (.catch (fn [_]
                  (when (= key @router/shown)
                    (dispatch! [:wider-failed])))))))

(defn refresh-filters!
  "Ask, once the corpus selection has held still for
  `filters-debounce-ms`, whether the metadata filters want fetching:
  `[:filters-due]` through `dispatch!`."
  [dispatch!]
  (debounce! filters-timer filters-debounce-ms #(dispatch! [:filters-due])))

(defn navigate!
  "Fetch the route at `href` as data and dispatch its arrival through
  `dispatch!`: `[:page-arrived data landed push?]`, `landed` being where
  the answer came from and `push?` whether it gets a history entry;
  `[:pending]` once an answer is `pending-delay-ms` late. Falls back to a
  real navigation on any failure, so a route the client cannot render is
  still a working page.

  A path is resolved first: everything past here reads an href with
  `js/URL`, which takes no relative one, and the fallback would answer
  the throw by reloading the page it was avoiding."
  [dispatch! href push?]
  (let [href       (.-href (js/URL. href js/location.href))
        controller (js/AbortController.)]
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

(defn align-pager!
  "Start pager `el` where the strip the concordance is read in starts
  (see `reading-strip`), so that its middle stands under the match at
  rest rather than in the middle of the region.

  A width, not an attribute, so it is written on render as the other
  measured things are. Nothing to line up with starts it at the edge."
  [el]
  (let [cpos (some-> (.querySelector js/document ".kwic .kwic-cpos")
                     (.getBoundingClientRect)
                     (.-width))]
    (.setProperty (.-style el) "padding-inline-start" (str (or cpos 0) "px"))))

(defn go-to-page!
  "Go to page `n` of the result on screen, as its pager's own links do:
  the URL in the bar with the page named in it, landing on the answer.

  The bar holds the citation the server wrote, which is the one thing
  here that knows every param the search was asked with. The first page
  is the default and no canonical URL says it."
  [dispatch! n]
  (let [url    (js/URL. js/location.href)
        params (.-searchParams url)]
    (if (= "1" n)
      (.delete params "page")
      (.set params "page" n))
    (navigate! dispatch! (str (.-pathname url) (.-search url)
                              url/results-fragment)
               true)))

(defn set-cookie!
  "Store `v` under setting `k` in the cookie the server reads (see
  dk.cst.corpus-probe.url/cookie), so a reload and every later visit
  carry the setting too."
  [k v]
  (set! (.-cookie js/document) (url/cookie k v)))

(defn reduced-motion?
  "True when the reader has asked their system for less animation."
  []
  (.-matches (js/matchMedia "(prefers-reduced-motion: reduce)")))

(defn keep-in-view!
  "Scroll the page so that `el` is on screen: a step of the cursor down
  the rows can land past the foot of the window. The panel is the foot
  while it is a sheet across the window, or it would cover the very token
  it describes."
  [el]
  (let [panel  (.getElementById js/document concordance/inspector-id)
        margin 16
        foot   (- (if (and panel (= "fixed" (.-position
                                             (js/getComputedStyle panel))))
                    (.-top (.getBoundingClientRect panel))
                    (.-innerHeight js/window))
                  margin)
        box    (.getBoundingClientRect el)]
    (when-let [dy (cond (< (.-top box) margin)  (- (.-top box) margin)
                        (> (.-bottom box) foot) (- (.-bottom box) foot))]
      (.scrollBy js/window #js {:top      dy
                                :behavior (if (reduced-motion?)
                                            "auto"
                                            "smooth")}))))

(defn focus!
  "Move focus to the element with `id`, once the render that put it there
  has run. `follow?` brings it onto the screen here rather than letting
  focus jump there itself (see `keep-in-view!`), which is what keeps the
  panel from covering the token it describes."
  ([id]
   (focus! id false))
  ([id follow?]
   (when-let [el (.getElementById js/document id)]
     (.focus el #js {:preventScroll follow?})
     (when follow? (keep-in-view! el)))))

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

(defn reading-strip
  "The strip that the reader reads in, as [start end] in viewport pixels.

  The strip starts inside the position column, which is pinned over the
  start of concordance region `el`."
  [el]
  (let [box (.getBoundingClientRect el)
        cpos (some-> (.querySelector el ".kwic-cpos")
                     (.getBoundingClientRect)
                     (.-width))]
    [(+ (.-left box) (or cpos 0)) (.-right box)]))

(defn centre-on!
  "Scroll concordance region `el` until `cell` is in the middle of the
  strip `[start end]` that is read (see `reading-strip`).

  Scroll by the difference between the two centres. The region thus stays
  where it is if it is already centred. `glide?` moves there slowly
  instead of immediately. A reader who asked for less animation always
  gets the immediate move."
  [el [start end] cell glide?]
  (let [box (.getBoundingClientRect cell)]
    (.scrollTo el #js {:left     (+ (.-scrollLeft el)
                                    (- (+ (.-left box) (/ (.-width box) 2))
                                       (/ (+ start end) 2)))
                       :behavior (if (and glide? (not (reduced-motion?)))
                                   "smooth"
                                   "auto")})))

(defn in-strip?
  "True when `cell` is anywhere in the strip `[start end]` that is read."
  [[start end] cell]
  (let [box (.getBoundingClientRect cell)]
    (and (< (.-left box) end) (> (.-right box) start))))

(defn centre-match!
  "Keep the token with id `token`, which the cursor is on, in the middle
  of concordance region `el`.

  Centre it again when one of these changes:
  - the token;
  - the page's `reach`;
  - the width of the strip that is read.
  Centre it again also when the cursor has left that strip. A render that
  changed none of these leaves the reader where they scrolled to.

  The region glides only between two tokens of one page at one width. A
  step along the line then reads as a step. In the other cases the region
  moves immediately. A new page, a page the reader came back to, and a
  page that changed width are all too far for a step."
  [el token reach]
  (when-let [cell (some->> token (.getElementById js/document))]
    (let [[start end :as strip] (reading-strip el)
          was    @centred
          ;; the width of the strip, and not of the region: the strip is
          ;; what the cursor is held in the middle of, and the position
          ;; column pinned over the region's edge is no part of it
          now    [token reach (- end start)]
          lost?  (not (in-strip? strip cell))
          ;; only the token changed, so the reader took a step
          glide? (and (not lost?) (= (rest now) (rest was)))]
      (when (or lost? (not= now was))
        (reset! centred now)
        (centre-on! el strip cell glide?)))))

(defn recentre!
  "Put the cursor of `state` back in the middle of the concordance. The
  window has changed size under it.

  Move immediately, and do not glide. A resize is not a step, and it
  happens many times while the reader drags a window edge.

  Take the cursor as the view resolves it, and not as the state holds it.
  The state has no cursor until the reader moves one. The view shows the
  default cursor until then."
  [state]
  (when-let [el (.getElementById js/document concordance/region-id)]
    (let [hits   (get-in state [:result :hits] [])
          cursor (concordance/resolved-cursor hits (:cursor state))]
      (when-let [cell (some->> (concordance/cursor-id cursor)
                               (.getElementById js/document))]
        (centre-on! el (reading-strip el) cell false)))))

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
  if they are not already at hand; else at the start of the main content.

  Focus, not only a scroll: a routed navigation replaces the page with
  none of the announcement and reset of focus a real one gives, leaving
  a reader who is not watching the screen told nothing and one on the
  keyboard in a page that has gone."
  []
  ;; nothing has gone where a control of the search form still holds
  ;; focus: the form outlives a search, and taking the caret out of the
  ;; field the reader typed in is no rescue
  (let [hash   (.-hash js/location)
        target (.getElementById js/document url/results-id)
        held?  (= url/form-id (some-> js/document (.-activeElement)
                                      (.-form) (.-id)))]
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
          (when-not held?
            (.focus target #js {:preventScroll true})))

      :else
      (do (.scrollTo js/window 0 0)
          (when-not held?
            (some-> (.getElementById js/document widgets/main-id)
                    (.focus #js {:preventScroll true})))))))

(defn sync-url!
  "Write the canonical query string of the page on screen into the bar,
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
          params (router/location-params)]
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
      :fetch-filters      (apply fetch-filters! dispatch! args)
      :fetch-counts       (fetch-counts! dispatch! state)
      :fetch-wider        (apply fetch-wider! dispatch! args)
      :refresh-filters    (refresh-filters! dispatch!)
      :navigate           (apply navigate! dispatch! args)
      :go-to-page         (apply go-to-page! dispatch! args)
      :align-pager        (align-pager! (:replicant/node data))
      :set-cookie         (apply set-cookie! args)
      :push-url           (apply push-url! args)
      :set-title          (apply set-title! args)
      :set-lang           (apply set-lang! args)
      :resubmit           (apply resubmit! args)
      :set-validity       (apply set-validity! (:replicant/node data) args)
      :set-checkbox-state (apply set-checkbox-state! (:replicant/node data)
                                 args)
      :centre-match       (apply centre-match! (:replicant/node data) args)
      :recentre           (recentre! state)
      :leave-concordance  (leave-concordance! dispatch!)
      :land               (land!)
      :sync-url           (sync-url! state))))
