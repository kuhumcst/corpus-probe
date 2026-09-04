(ns dk.cst.corpus-probe.ui
  "Client entry point: read the bootstrap payload the server embedded,
  mount Replicant on the server-rendered page, and take over navigation so
  moving between views swaps the page's data rather than reloading it.

  Every route the server renders it also serves as transit, from the same
  handler, so the two can never describe different pages. A link click or
  a GET submit is fetched as data, rendered through the same .cljc views,
  and pushed onto the history; anything that fails falls back to a real
  navigation, which works because the server still renders every page in
  full.

  Interactivity is progressive: without this script the page is a working
  server-rendered concordance, a token is text rather than a control, and
  the browser lands the reader on the results from the fragment on the
  form action. With it, the same views become live and `land!` does by
  hand what the browser did for free. The set of expanded hits is mirrored
  in the URL's `expand` parameter, so an expanded view survives a reload
  and can be shared."
  (:require [clojure.string :as str]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.views.kwic :as kwic]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]
            [dk.cst.corpus-probe.views.app :as app-views]
            [replicant.dom :as r]))

(defonce state
  (atom nil))

(def arrow-keys
  "How each key moves the concordance's cursor: [rows tokens], a step
  between hits and a step within one."
  {"ArrowRight" [0 1]
   "ArrowLeft"  [0 -1]
   "ArrowDown"  [1 0]
   "ArrowUp"    [-1 0]})

(def transit-type
  "The content type the server answers a route with."
  "application/transit+json")

(defn read-transit
  "Decode transit-JSON string `s`."
  [s]
  (transit/read (transit/reader :json) s))

(defn read-payload
  "Read the transit payload from the #bootstrap script element, or nil on a
  page the server shipped none for."
  []
  (some-> (.getElementById js/document "bootstrap")
          (.-textContent)
          (read-transit)))

(defn current-url
  "The current location as a mutable URL object."
  []
  (js/URL. js/location.href))

(defn collapse!
  "Remove the hit keyed `k` from the expanded set if it is still there."
  [k]
  (when (contains? (:expanded @state) k)
    (swap! state update :expanded dissoc k)))

(defn fetch-context!
  "Fetch the hit at `cpos`/`matchend` in `corpus` with wider context and, if
  it is still wanted, store it under its key in `:expanded`.

  A failed request or a hit the user collapsed while the fetch was in flight
  collapses the entry again, so a late response never revives a hit the user
  dismissed."
  [corpus cpos matchend]
  (let [k [corpus cpos]]
    (-> (js/fetch (str "/api/context?corpus=" corpus
                       "&cpos=" cpos "&matchend=" matchend))
        (.then (fn [response]
                 (if (.-ok response)
                   (.text response)
                   (throw (js/Error. "context request failed")))))
        (.then (fn [body]
                 (let [hit (transit/read (transit/reader :json) body)]
                   (if (and hit (contains? (:expanded @state) k))
                     (swap! state assoc-in [:expanded k] hit)
                     (collapse! k)))))
        (.catch (fn [_]
                  (if (contains? (:expanded @state) k)
                    (swap! state assoc-in [:expanded k] kwic/failed)
                    (collapse! k)))))))

(defn hits
  "The hits of the page being shown, in the order they are rendered."
  []
  (get-in @state [:result :hits] []))

(defn focus-cursor!
  "Move focus to the token the cursor is on, once the render that put it
  there has run."
  []
  (let [[k i] (:cursor @state)
        [corpus cpos] k]
    (some-> (.getElementById js/document
                             (kwic/token-id {:corpus corpus :cpos cpos} i))
            (.focus))))

(defn cursor-rows
  "Every row the cursor can visit, in the order they are read: each hit's
  own row and, beneath it, its wider context while one is showing.

  A row is {:key <hit-key> :hit <the hit holding its tokens> :from <the
  index its first token carries>}. An expanded row numbers its tokens past
  the row it expands, so one index names one token across both."
  []
  (let [expanded (:expanded @state)]
    (mapcat (fn [hit]
              (let [k  (kwic/hit-key hit)
                    ex (get expanded k)]
                (cond-> [{:key k :hit hit :from 0}]
                  (map? ex) (conj {:key  k
                                   :hit  ex
                                   :from (kwic/token-count hit)}))))
            (hits))))

(defn row-at
  "The index in `rows` of the row the cursor `[k i]` is in: the last row of
  that hit whose tokens start at or before `i`."
  [rows [k i]]
  (or (last (keep-indexed (fn [n {:keys [key from]}]
                            (when (and (= key k) (<= from i)) n))
                          rows))
      0))

(defn step-cursor
  "The cursor moved `[rows tokens]` through `rows*` (see `cursor-rows`).

  Along a row the cursor stops at its ends rather than wrapping. Between
  rows it keeps its distance from the match rather than its column, so
  stepping into a hit's wider context lands on the word the cursor was
  already on and stepping out lands back where it came from. Rows hold
  different amounts of text, so an offset only the wider one has is
  answered with its nearest token."
  [rows* cursor [rows tokens]]
  (let [at   (row-at rows* cursor)
        here (nth rows* at)
        i    (- (second cursor) (:from here))]
    (if (zero? rows)
      (let [i* (min (dec (kwic/token-count (:hit here))) (max 0 (+ i tokens)))]
        [(:key here) (+ (:from here) i*)])
      (let [n      (min (dec (count rows*)) (max 0 (+ at rows)))
            there  (nth rows* n)
            offset (kwic/token-offset (:hit here) i)]
        [(:key there)
         (+ (:from there) (kwic/offset-token (:hit there) offset))]))))

(defn move-cursor!
  "Answer a key pressed on the token at cursor `k`: an arrow moves the
  cursor and focus with it, Home and End go to the ends of the row the
  cursor is in, and Escape closes the panel.

  The concordance is one tab stop with a cursor inside it, so the arrow
  keys have to be handled here; the browser has no meaning of its own for
  them on a button, which is why each is consumed."
  [event k]
  (let [pressed (.-key event)
        rows    (cursor-rows)]
    (cond
      (= "Escape" pressed)
      (do (.preventDefault event)
          (swap! state dissoc :selected))

      (contains? arrow-keys pressed)
      (do (.preventDefault event)
          (swap! state assoc :cursor
                 (step-cursor rows k (arrow-keys pressed)))
          (focus-cursor!))

      ;; the ends of the row the cursor is in, not of the hit: a wider
      ;; context is its own run of text
      (contains? #{"Home" "End"} pressed)
      (let [{:keys [key hit from]} (nth rows (row-at rows k))
            i (if (= "Home" pressed) 0 (dec (kwic/token-count hit)))]
        (.preventDefault event)
        (swap! state assoc :cursor [key (+ from i)])
        (focus-cursor!))

      :else nil)))

(defn close-panel!
  "Dismiss the inspection panel and leave focus in the concordance it
  describes, rather than on a token.

  Focus cannot go back to a token: the panel follows focus, so focusing
  one would open the panel again, which is what closing it from any token
  but the cursor's used to do. It cannot stay where it is either, since
  the button it is on is about to stop existing. So it goes to the
  concordance itself, which is focusable because it scrolls, and a tab
  from there reaches the cursor again.

  Found by its own id rather than by the class the stylesheet uses, so
  renaming a style hook cannot quietly leave focus on the body."
  []
  (swap! state dissoc :selected)
  (some-> (.getElementById js/document kwic/region-id) (.focus)))

(defn leave-concordance!
  "Close the inspection panel once focus has settled outside both `region`
  and the panel itself.

  The panel counts as part of inspecting, not as somewhere else: it sits
  before the concordance in the document, so shift-tabbing off a token
  lands on its close button, and a panel that closed on the way in would
  pull the ground from under the focus arriving.

  Deferred by a tick because focusout fires before the next element has
  focus, and read from `activeElement` rather than the event's
  relatedTarget so that clicking the page background closes the panel
  while merely switching windows does not: a blurred window keeps its
  active element, an abandoned concordance does not."
  [region]
  (js/setTimeout
   (fn []
     (let [el    (.-activeElement js/document)
           panel (.querySelector js/document "aside.sidebar")]
       (when-not (or (.contains region el)
                     (and panel (.contains panel el)))
         (swap! state dissoc :selected))))
   0))

(defn handle!
  "Apply an `action` to the state, using `data` (the Replicant dispatch
  data) for the key a token was pressed with.

  `:inspect` fires on focus as well as on click, so the panel describes
  whatever the cursor is on rather than waiting for a press.
  `:move-cursor` answers a key pressed on a token, `:leave-concordance`
  closes the panel once focus has gone elsewhere, and `:close` dismisses
  it from its own button. `:toggle-context`
  expands a hit (fetching its wider context) or collapses it. `:set-mode`
  records the query mode the reader picked, which swaps the query example
  the placeholder shows.

  Re-rendering the form does not disturb what the reader has typed: the
  query input's value is the same in both renders, so Replicant leaves the
  element alone."
  [data [action arg]]
  (case action
    :set-mode (swap! state assoc-in [:params :mode] arg)
    :inspect (swap! state assoc :selected arg)
    :close   (close-panel!)
    :move-cursor (move-cursor! (:replicant/dom-event data) arg)
    :leave-concordance
    (leave-concordance! (.-currentTarget (:replicant/dom-event data)))
    :toggle-context
    (let [{:keys [corpus cpos matchend]} arg
          k (kwic/hit-key arg)]
      (if (contains? (:expanded @state) k)
        (swap! state update :expanded dissoc k)
        ;; commit intent immediately (loading placeholder) so the toggle and
        ;; the URL reflect the click at once and a duplicate fetch is suppressed
        (do (swap! state assoc-in [:expanded k] ::loading)
            (fetch-context! corpus cpos matchend))))
    nil))

(defn expand-param
  "The set of hit keys named in the URL's `expand` parameter (`CORPUS:cpos`
  items); malformed items are ignored."
  []
  (when-let [param (.get (.-searchParams (current-url)) "expand")]
    (set (keep (fn [item]
                 (let [[corpus cpos] (str/split item #":" 2)]
                   (when-let [n (some-> cpos parse-long)]
                     [corpus n])))
               (str/split param #",")))))

(defn sync-expand-url!
  "Mirror the expanded hits in the URL's `expand` parameter as
  `CORPUS:cpos` items, replacing history so the URL stays shareable without
  new entries; a no-op when the URL already says that."
  []
  (let [url (current-url)
        ks  (sort (keys (:expanded @state)))]
    (if (seq ks)
      (.set (.-searchParams url) "expand"
            (str/join "," (map (fn [[corpus cpos]] (str corpus ":" cpos))
                               ks)))
      (.delete (.-searchParams url) "expand"))
    ;; only when it would say something new: this runs on every render, and
    ;; a render happens on every arrow key. Safari throws past roughly a
    ;; hundred history writes in thirty seconds, and that throw comes back
    ;; out through the watcher into the swap! that moved the cursor
    (when (not= (.-href url) js/location.href)
      (.replaceState js/history nil "" (.-href url)))))

(defn render!
  "Render the current state into the masthead and #app, then sync the URL
  to it."
  []
  ;; The first render clears #app and rebuilds the server-rendered markup
  ;; rather than adopting it, so a query typed, a disclosure opened or
  ;; focus taken while the script loads is lost. This is not a passing
  ;; state of the library: replicant.dom/render promises to replace
  ;; whatever it finds, there is no adoption API in 2026.07.1, and the
  ;; upstream request for one (issue 53) has been open since March 2025
  ;; with no commitment. If it ever arrives it has to be a new entry
  ;; point. The cheap mitigation, should the window ever matter, is to
  ;; read #q's live value here before rendering.
  (let [{:keys [lang path nav] :as state} @state]
    ;; the masthead's links carry the current search, so it re-renders with
    ;; the page rather than keeping whatever the first server render said
    (r/render (.getElementById js/document "masthead")
              (layout/site-header lang path nav))
    (r/render (.getElementById js/document "app") (app-views/page state)))
  (sync-expand-url!))

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
  "Put the reader where a routed navigation should leave them: focused on
  the results the URL names, and moved to them only if they are not
  already on screen.

  Focus always moves, because that is what tells a reader the outcome
  arrived. Scrolling only happens when the results are not already at
  hand: switching the view of a result, or searching again beside one,
  leaves the page where it is, while a turn of the page from the foot of a
  long one, or a first search on a narrow screen, brings them up. The
  browser scrolls on a real navigation, from the fragment on the form
  action; pushState does not, so the client decides."
  [url]
  (let [target (when (= (str "#" page/results-id) (.-hash url))
                 (.getElementById js/document page/results-id))]
    (cond
      (nil? target)        (.scrollTo js/window 0 0)
      (at-hand? target)    (.focus target #js {:preventScroll true})
      :else                (do (.scrollIntoView target)
                               (.focus target #js {:preventScroll true})))))

(defn wanted-hits
  "The hits of `data` whose key is in `wanted` (nil when nothing is
  wanted); `?expand` is scoped to one page, so off-page hits are ignored."
  [data wanted]
  (when wanted
    (filter (comp wanted kwic/hit-key) (get-in data [:result :hits]))))

(defn with-expansions
  "`data` with the hits the URL asks to expand seeded as loading
  placeholders.

  Seeded before the render rather than after it, because that render
  mirrors the expanded set back into the URL, and an empty set would erase
  the `expand` parameter it is about to be restored from. That is what
  used to lose an expanded view on the back button."
  [data]
  (let [hits (wanted-hits data (expand-param))]
    (cond-> data
      (seq hits) (assoc :expanded
                        (into {}
                              (map (fn [hit] [(kwic/hit-key hit) ::loading]))
                              hits)))))

(defn fetch-expansions!
  "Fetch the wider context of every hit currently placeheld in `:expanded`."
  []
  (doseq [[[corpus cpos] v] (:expanded @state)
          :when (= ::loading v)
          :let [hit (first (filter #(= [corpus cpos] (kwic/hit-key %))
                                   (hits)))]
          :when hit]
    (fetch-context! corpus cpos (:matchend (:anchors hit)))))

(defn navigate!
  "Fetch the route at `href` as data and render it, without a page load.

  Falls back to a real navigation on any failure, so a route the client
  cannot render is still a working page: the server renders every one of
  them. `push?` adds a history entry; a popstate replaces nothing.

  Any expansion the URL names is restored, so going back to a page whose
  hits were expanded shows them expanded again."
  [href push?]
  (-> (js/fetch href #js {:headers #js {"Accept" transit-type}})
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (throw (js/Error. "route request failed")))))
      (.then (fn [body]
               (let [data (read-transit body)
                     url  (js/URL. href js/location.href)]
                 (when push? (.pushState js/history nil "" href))
                 (set! (.-title js/document) (:title data))
                 (reset! state (with-expansions (assoc data :client? true)))
                 (fetch-expansions!)
                 (land! url))))
      (.catch (fn [_] (set! (.-href js/location) href)))))

(defn set-preference!
  "Store `v` under setting `k` and fetch this page again with it applied.

  The same cookie the server reads, so a reload and every later visit
  carry the setting too; and a fetch rather than a re-render, because the
  server words the document title and the result summaries, not the
  client. The server decides what a setting accepts, so nothing is
  validated here: a reader can only mislead themselves."
  [k v]
  (set! (.-cookie js/document)
        (str k "=" v ";Path=/;Max-Age=31536000;SameSite=Lax"))
  (navigate! js/location.href false))

(def routable-paths
  "The paths the client renders itself. Anything else stays the browser's
  business: an export is a download rather than a page, and fetching one
  as data would run the search a second time and then hand the reader
  nothing."
  #{"/" "/corpora"})

(defn routable?
  "True when `url` names a page this client knows how to render."
  [url]
  (let [path (.-pathname url)]
    (or (contains? routable-paths path)
        (str/starts-with? path "/corpus/"))))

(defn routed?
  "True when `el` is a link this app renders itself: a page it knows, same
  origin, no target and no modifier, so the browser's own meaning of the
  click is kept."
  [el event]
  (and el
       (= (.-origin (js/URL. (.-href el))) js/location.origin)
       (routable? (js/URL. (.-href el)))
       (str/blank? (.-target el))
       (not (or (.-metaKey event) (.-ctrlKey event)
                (.-shiftKey event) (.-altKey event)))
       (not= 1 (.-button event))))

(defn route-clicks!
  "Take over link clicks and form submits that stay inside the app, so
  moving between views keeps the state rather than reloading the document."
  []
  (.addEventListener
   js/document "click"
   (fn [e]
     (when-not (.-defaultPrevented e)
       (when-let [link (some-> (.-target e) (.closest "a[href]"))]
         (when (routed? link e)
           (.preventDefault e)
           (navigate! (.-href link) true))))))
  (.addEventListener
   js/document "submit"
   (fn [e]
     (let [form (.-target e)]
       (cond
         (.-defaultPrevented e) nil

         ;; the language switch changes a preference, not a place: store it
         ;; and ask the server for this same page in the other language,
         ;; rather than posting and reloading
         (= layout/preferences-path (.getAttribute form "action"))
         (let [b (.-submitter e)]
           (.preventDefault e)
           (set-preference! (.-name b) (.-value b)))

         :else
         (when (and (= "get" (str/lower-case (or (.-method form) "get")))
                    (= (.-origin (js/URL. (.-action form)))
                       js/location.origin))
         (.preventDefault e)
         ;; the browser's own rules for what a form submits, so a routed
         ;; submit sends exactly what a real one would
           (let [url (js/URL. (.-action form))
                 qs  (.toString (js/URLSearchParams. (js/FormData. form)))]
             (.preventDefault e)
             (set! (.-search url) qs)
             (navigate! (.-href url) true)))))))
  (.addEventListener js/window "popstate"
                     (fn [_] (navigate! js/location.href false))))

(defn init!
  "Boot the client: seed state from the bootstrap payload, wire dispatch and
  routing, re-render on every state change, and restore any expansions the
  URL names, exactly as a routed navigation does."
  []
  ;; the views render tokens and disclosures as controls only where this
  ;; script is running to answer them
  (reset! state (with-expansions (assoc (read-payload) :client? true)))
  (route-clicks!)
  (r/set-dispatch! handle!)
  (add-watch state ::render (fn [_ _ _ _] (render!)))
  (render!)
  (fetch-expansions!))
