(ns dk.cst.corpus-probe.client.effects
  "The edge of the client: everything that touches the world, the timers,
  the fetches, the history, the document's title and language, the
  cookie, the store the searches made lately are kept in, the moves of
  focus (see dk.cst.corpus-probe.client.focus) and `perform!`, which
  runs the effects an action answered with. Nothing here reads or writes
  the state: a fetch comes back as an action, so what to keep of a late
  answer is the pure step's decision."
  (:require [cognitect.transit :as transit]
            [dk.cst.corpus-probe.client.focus :as focus]
            [dk.cst.corpus-probe.client.router :as router]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.storage :as storage]
            [dk.cst.corpus-probe.storage.recent :as recent]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.concordance :as concordance]))

(defonce ^{:doc "The debounces waiting to fire, by name (see `debounce!`):
  the report of a routed navigation, a metadata filter refresh, a page
  turn and a view being applied."}
  timers
  (atom {}))

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

(def settle-ms
  "How long a control must hold still before what it asks for is asked
  (see `debounce!`)."
  ;; a closed select reports every option a key passes over, so arrowing
  ;; from page one to four would otherwise ask for three pages. Set by
  ;; the reader who taps: longer than the gap between two presses,
  ;; shorter than reads as lag once they have stopped
  200)

(defn read-transit
  "Decode transit-JSON string `s`."
  [s]
  (transit/read (transit/reader :json) s))

(defn cancel!
  "Call off whatever the debounce named `k` was waiting to do."
  [k]
  (some-> (get @timers k) js/clearTimeout)
  (swap! timers dissoc k))

(defn debounce!
  "Call `f` after `ms`, cancelling whatever the debounce named `k` was
  already waiting to do: for a control that fires while a reader is
  still working it, only the state they stop on is worth acting on."
  [k ms f]
  (cancel! k)
  (swap! timers assoc k (js/setTimeout f ms)))

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

(defn on-this-page
  "`f`, called only while the page it was made on is still the one on
  screen (see dk.cst.corpus-probe.client.router/shown): a late answer to
  a page the reader has left is dropped."
  [f]
  (let [key (router/page-key)]
    (fn [x] (when (= key @router/shown) (f x)))))

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
    ;; the page's own query string, so the server counts the question
    ;; the page answered
    (-> (fetch-transit! (str url/counts-api js/location.search))
        (.then (on-this-page (fn [[counted]]
                               (dispatch! [:counts-arrived counted]))))
        ;; a real navigation, as a page that fails: the server renders
        ;; the page with its count in full
        (.catch (on-this-page (fn [_]
                                (set! (.-href js/location)
                                      js/location.href)))))))

(defn fetch-wider!
  "Fetch the page on screen holding `reach` words either side of each
  match and dispatch its arrival through `dispatch!`: `[:wider-arrived
  reach data]`. The page's own query string with the reach added, so the
  server answers the question the page answered; the URL in the bar is
  left as it is, a reach being a way of reading a result rather than part
  of naming one. Dropped once the reader has moved on."
  [dispatch! reach]
  (let [url (router/current-url)]
    (.set (.-searchParams url) "reach" (str reach))
    (-> (fetch-transit! (str (.-pathname url) (.-search url)))
        (.then (on-this-page (fn [[data]]
                               (dispatch! [:wider-arrived reach data]))))
        (.catch (on-this-page (fn [_] (dispatch! [:wider-failed])))))))

(defn refresh-filters!
  "Ask, once the corpus selection has held still (see `settle-ms`),
  whether the metadata filters want fetching: `[:filters-due]` through
  `dispatch!`."
  [dispatch!]
  (debounce! :filters settle-ms #(dispatch! [:filters-due])))

(defn navigate!
  "Fetch the route at `href` as data and dispatch its arrival through
  `dispatch!`: `[:page-arrived data landed push?]`, `landed` being where
  the answer came from and `push?` whether it gets a history entry;
  `[:pending]` once an answer is `pending-delay-ms` late. Falls back to a
  real navigation on any failure, so a route the client cannot render is
  still a working page."
  [dispatch! href push?]
  ;; resolved first: js/URL takes no relative href, and the fallback
  ;; would answer the throw by reloading the page it was avoiding
  (let [href       (.-href (js/URL. href js/location.href))
        controller (js/AbortController.)]
    ;; called off rather than raced, so the reader gets the answer to
    ;; their latest question
    (some-> @in-flight (.abort))
    (reset! in-flight controller)
    (debounce! :pending pending-delay-ms #(dispatch! [:pending]))
    (-> (fetch-transit! href (.-signal controller))
        (.then (fn [[data landed]]
                 ;; before the state is replaced, or a report scheduled
                 ;; for a wait that is over lands on the answer to it
                 (cancel! :pending)
                 (dispatch! [:page-arrived data (router/landed-href href landed)
                             push?])))
        (.catch (fn [_]
                  ;; an abort is no failure to fall back from, and its
                  ;; timer belongs to the navigation that did the
                  ;; aborting, still in flight and maybe worth reporting
                  (when-not (.-aborted (.-signal controller))
                    (cancel! :pending)
                    (set! (.-href js/location) href)))))))

(defn align-pager!
  "Start pager `el` where the strip the concordance is read in starts
  (see `reading-strip`), so that its middle stands under the match at
  rest rather than in the middle of the region; at the edge when there
  is nothing to line up with."
  [el]
  (let [cpos (some-> (.querySelector js/document ".kwic .kwic-cpos")
                     (.getBoundingClientRect)
                     (.-width))]
    ;; set on the style object and never as a style attribute string,
    ;; which style-src 'self' refuses to parse
    (.setProperty (.-style el) "padding-inline-start" (str (or cpos 0) "px"))))

;; TODO: a hack for Firefox, which has no scroll-driven animations; remove
;; it when CSS.supports there says true to the query below
(defn fade-edges!
  "Write the fade widths of scroll region `el` that the stylesheet
  animates elsewhere (style.css, `.scroll:has(> .frequencies)`) on its
  scroll, its resize and each render, when its table may be another width."
  [^js el]
  (when-not (.supports js/CSS "animation-timeline: scroll(self inline)")
    (when-not (.-fadeEdges el)
      (let [style    (.-style el)
            ;; read once: a computed style on every scroll event is the
            ;; cost this must not have
            fade     (js/parseFloat (.getPropertyValue (js/getComputedStyle el)
                                                       "--fade"))
            update!  (fn []
                       (let [left (.-scrollLeft el)
                             most (- (.-scrollWidth el) (.-clientWidth el))]
                         ;; past the first few pixels of scroll nothing
                         ;; changes, and a scroll event then writes nothing
                         (doseq [[k v] {"--fade-start" (min left fade)
                                        "--fade-end"   (max 0 (min (- most left)
                                                                   fade))}
                                 :let [px (str v "px")]
                                 :when (not= px (.getPropertyValue style k))]
                           (.setProperty style k px))))
            observer (js/ResizeObserver. update!)]
        (set! (.-fadeEdges el) update!)
        (set! (.-fadeEdgesObserver el) observer)
        (.addEventListener el "scroll" update! #js {:passive true})
        (.observe observer el)))
    ((.-fadeEdges el))))

(defn unfade-edges!
  "Stop watching the size of scroll region `el` when it leaves the page
  (see `fade-edges!`)."
  [^js el]
  ;; an observer keeps its target, table and all
  (some-> (.-fadeEdgesObserver el) (.disconnect)))

(defn go-to-page!
  "Go to page `n` of the result on screen once the select has held still
  (see `settle-ms`), as its pager's own links do: the URL in the bar with
  the page named in it, landing on the answer."
  [dispatch! n]
  ;; the bar holds the citation the server wrote, the one thing here that
  ;; knows every param the search was asked with
  (let [url    (js/URL. js/location.href)
        params (.-searchParams url)]
    (if (= "1" n)
      (.delete params "page")
      (.set params "page" n))
    (debounce! :page settle-ms
               #(navigate! dispatch!
                           (str (.-pathname url) (.-search url)
                                url/results-fragment)
                           true))))

(defn set-cookie!
  "Store `v` under setting `k` in the cookie the server reads (see
  dk.cst.corpus-probe.storage/cookie), so a reload and every later visit
  carry the setting too."
  [k v]
  (set! (.-cookie js/document) (storage/cookie k v)))

(defn read-recent!
  "The searches the browser remembers (see
  dk.cst.corpus-probe.storage.recent/entries); none where it keeps no
  store, a browser told to keep none throwing at the reading of it."
  []
  (recent/entries (try
                    (.getItem js/localStorage recent/store-key)
                    (catch :default _ nil))))

(defn store-recent!
  "Store the history `state` holds, so a later visit finds it; a store
  that refuses leaves the history to this visit, since no answer depends
  on it."
  [state]
  (try
    (.setItem js/localStorage recent/store-key (recent/string (:recent state)))
    (catch :default _ nil)))

(defn reduced-motion?
  "True when the reader has asked their system for less animation."
  []
  (.-matches (js/matchMedia "(prefers-reduced-motion: reduce)")))

(defn sheet?
  "True when the inspection `panel` is the sheet across the foot of the
  window rather than the card anchored under the token: the stylesheet
  gives only the card a position area, and a browser that knows no such
  property shows the sheet."
  [panel]
  (contains? #{"" "none"}
             (.getPropertyValue (js/getComputedStyle panel) "position-area")))

(defn mark-side!
  "Say on the inspection panel which side of its token the browser put
  it, as the attributes `data-above` and `data-left`, which the
  stylesheet squares the joining corners by: a card anchored under the
  token flips over it, or to its left, where the window runs out, and
  no rule can read which fallback was taken. Nothing to say without the
  card or the token."
  []
  (when-let [panel (.getElementById js/document concordance/inspector-id)]
    (when-let [token (.querySelector js/document
                                     ".kwic .token[tabindex=\"0\"]")]
      (let [card  (.getBoundingClientRect panel)
            token (.getBoundingClientRect token)]
        ;; a pixel of slack: the card overlaps the token's outline, and
        ;; the flipped card ends where the token begins
        (.toggleAttribute panel "data-above"
                          (<= (.-bottom card) (+ (.-top token) 1)))
        (.toggleAttribute panel "data-left"
                          (< (.-left card) (- (.-left token) 8)))))))

(defn keep-in-view!
  "Scroll the page so that `el` is on screen: a step of the cursor down
  the rows can land past the foot of the window. The panel is the foot
  while it is a sheet across the window, or it would cover the very token
  it describes."
  [el]
  (let [panel  (.getElementById js/document concordance/inspector-id)
        margin 16
        foot   (- (if (and panel (sheet? panel))
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

(defn follow-cursor!
  "Move focus to the cursor's token with `id` and bring it onto the
  screen here (see `keep-in-view!`) rather than letting focus jump there
  itself, which is what keeps the panel from covering the token it
  describes."
  [id]
  (some-> (focus/focus! id true) (keep-in-view!)))

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
  "Submit the form with `form-id` again, as it now stands, at once: what
  a reader asks for themselves, by pressing Enter, is not waited on.

  Through the form rather than a built URL, so that the sort or the
  grouping travels with everything else the form holds, by the routed
  path a press would take."
  [form-id]
  (some-> (.getElementById js/document form-id) (.requestSubmit)))

(defn apply-view!
  "Submit the form with `form-id` for a change to view control `node`:
  once it has held still where it is a select (see `settle-ms`), at once
  where it is anything else."
  [node form-id]
  ;; only a select reports what is still being worked; a box or a field
  ;; reports a decision made, and waiting on those is lag for nothing
  (if (= "SELECT" (.-tagName node))
    (debounce! :view settle-ms #(resubmit! form-id))
    (resubmit! form-id)))

(defn prevent-default!
  "Keep the browser from answering `event` itself."
  [event]
  (.preventDefault event))

(defn set-validity!
  "Write `msg`, the constraint the markup cannot state, to control `node`:
  the message it reports, or nothing for nil."
  [node msg]
  (.setCustomValidity node (or msg "")))

(defn set-indeterminate!
  "Write to checkbox `node` whether it is `indeterminate`, for a folder
  holding only part of the selection: no attribute carries the state, so
  it is written to the element on every render."
  [node indeterminate]
  (set! (.-indeterminate node) indeterminate))

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
  of concordance region `el`: centred again when the token, the page's
  `reach` or the width of the strip read changes, or the cursor has
  left the strip; a render that changed none of these leaves the reader
  where they scrolled to."
  [el token reach]
  ;; it glides only between two tokens of one page at one width, so a
  ;; step reads as a step; a new page or a new width is too far for one
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
  "Put the cursor of `state` back in the middle of the concordance after
  the window changed size under it, without gliding: a resize is no
  step, and it fires many times as a window edge is dragged."
  [state]
  ;; the cursor as the view resolves it: the state has none until the
  ;; reader moves one, and the view shows the default until then
  (when-let [el (.getElementById js/document concordance/region-id)]
    (let [hits   (get-in state [:result :hits] [])
          cursor (concordance/resolved-cursor hits (:cursor state))]
      (when-let [cell (some->> (concordance/cursor-id cursor)
                               (.getElementById js/document))]
        (centre-on! el (reading-strip el) cell false)))))

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
      :focus              (apply focus/focus! args)
      :focus-field        (apply focus/focus-field! args)
      :follow-cursor      (apply follow-cursor! args)
      :once-left          (apply focus/once-left! dispatch! args)
      :fetch-filters      (apply fetch-filters! dispatch! args)
      :fetch-counts       (fetch-counts! dispatch! state)
      :fetch-wider        (apply fetch-wider! dispatch! args)
      :refresh-filters    (refresh-filters! dispatch!)
      :navigate           (apply navigate! dispatch! args)
      :go-to-page         (apply go-to-page! dispatch! args)
      :align-pager        (align-pager! (:replicant/node data))
      :fade-edges         (fade-edges! (:replicant/node data))
      :unfade-edges       (unfade-edges! (:replicant/node data))
      :set-cookie         (apply set-cookie! args)
      :store-recent       (store-recent! state)
      :push-url           (apply push-url! args)
      :set-title          (apply set-title! args)
      :set-lang           (apply set-lang! args)
      :resubmit           (apply resubmit! args)
      :apply-view         (apply apply-view! (:replicant/node data) args)
      :set-validity       (apply set-validity! (:replicant/node data) args)
      :set-indeterminate  (apply set-indeterminate! (:replicant/node data) args)
      :centre-match       (apply centre-match! (:replicant/node data) args)
      :recentre           (recentre! state)
      :mark-side          (mark-side!)
      :land               (focus/land!)
      :select-query       (focus/select-query! state)
      :sync-url           (sync-url! state))))
