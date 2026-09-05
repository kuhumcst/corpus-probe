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
  and can be shared.

  Every URL this client writes, a submitted form's and the one carrying
  the expansions, goes through dk.cst.corpus-probe.url, so it is the URL
  the server would have written for the same search."
  (:require [clojure.string :as str]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.kwic :as kwic]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]
            [dk.cst.corpus-probe.views.app :as app-views]
            [replicant.dom :as r]))

(defonce state
  (atom nil))

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

(defonce shown
  ;; the path and query of the page on screen, so that a popstate leaving
  ;; them as they are, which is a jump to a fragment, is not taken for a
  ;; page to fetch again
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

(defn form-params
  "The fields of `form` as a submit would send them, as the params map
  the server reads them into (see `params-of`)."
  [form]
  (params-of (js/URLSearchParams. (js/FormData. form))))

(defn location-params
  "The query params of the current location (see `params-of`)."
  []
  (params-of (.-searchParams (current-url))))

(defn page-key
  "What names the page the location is on: its path and query, which a
  fragment is a place within."
  []
  (str js/location.pathname js/location.search))

(defn fragment
  "The place in the page the location names, its fragment without the
  mark; nil when it names none."
  []
  (let [hash js/location.hash]
    (when (seq hash)
      (js/decodeURIComponent (subs hash 1)))))

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
                 (let [hit (read-transit body)]
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

(defn select-corpora
  "The selected corpus IDs `corpus` with every ID in `ids` added when
  `add?`, and with all of them removed otherwise.

  Sorted, so that a selection reads the same however the reader arrived
  at it, and a set throughout, so that selecting a folder whose corpora
  are already selected cannot list one of them twice."
  [corpus ids add?]
  (let [selected (set corpus)]
    (vec (sort (if add? (into selected ids) (reduce disj selected ids))))))

(defn choose-values
  "`selected`, each metadata attribute mapped to the values chosen under
  it, with every value in `values` added under `attr`, or all of them
  taken away when they are all there already.

  One rule for one value and for every value of an attribute, as the
  corpus chooser has one rule for a corpus and for a folder: a box that is
  on turns off, and an attribute only partly chosen fills rather than
  clearing the part the reader already had.

  An attribute left with nothing chosen is dropped rather than kept empty,
  so that an attribute the reader has finished with does not go on being
  counted as one they are filtering by."
  [selected attr values]
  (let [chosen (set (get selected attr))
        chosen (if (every? chosen values)
                 (reduce disj chosen values)
                 (into chosen values))]
    (if (seq chosen)
      (assoc selected attr chosen)
      (dissoc selected attr))))

(defn toggle-corpora!
  "Select every corpus in `ids`, or clear them all when they are already
  selected.

  One rule serves a single corpus and a whole folder alike: a box that is
  on turns off, and a folder that is wholly selected clears, while a
  folder that is only partly selected fills rather than clearing the part
  of it the reader already had."
  [ids]
  (swap! state update-in [:params :corpus]
         (fn [corpus]
           (select-corpora corpus ids (not (every? (set corpus) ids))))))

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

(defn apply-view!
  "Submit the search again with the result's own controls as they now
  stand.

  At once, because a <select> reports the value a reader settled on
  rather than the ones they passed over on the way: choosing an order is
  asking for it, and any wait between the two is a wait nobody asked for.

  Through the form rather than by building a URL, so that the sort or the
  grouping travels with everything else the form holds and takes the same
  routed path a reader pressing the button would."
  []
  (some-> (.getElementById js/document page/form-id) (.requestSubmit)))

(def filters-debounce-ms
  "How long the corpus selection must hold still before the metadata
  filters it offers are fetched. Long enough that ticking several boxes in
  a row is one request, short enough that a reader who has stopped is not
  left waiting on a timer."
  300)

(defn chosen-corpora
  "The selected corpus IDs of `state` in a settled order, which is what
  the metadata filters are asked for and remembered by."
  [state]
  (vec (sort (get-in state [:params :corpus]))))

(defn filters-stale?
  "True when the metadata filters `state` holds are not the ones the
  corpora now selected offer, and the reader is looking at them.

  Only while they are open: filters nobody has opened are filters nobody
  has to fetch, and a corpus selection is usually changed several times
  before anyone asks what metadata it carries."
  [{:keys [filters-open? filters-for] :as state}]
  (and filters-open? (not= (chosen-corpora state) filters-for)))

(defn fetch-filters!
  "Fetch the metadata filters `corpora` offer and put them in the state,
  keeping whatever values the reader has already chosen.

  Only `:attrs` and `:unlisted` are replaced. `:selected` is the reader's,
  and a chosen value the new corpora do not offer keeps its checkbox (see
  dk.cst.corpus-probe.views.page/filter-details), so narrowing a selection
  never quietly drops part of a filter.

  A response is applied only while it still describes the selection, so a
  slow answer to a question the reader has moved on from is dropped rather
  than overwriting the answer to the one they are asking now."
  [corpora]
  (let [params (js/URLSearchParams.)]
    (doseq [corpus corpora] (.append params "corpus" corpus))
    (swap! state assoc :filters-pending? true)
    (-> (js/fetch (str "/api/filters?" params)
                  #js {:headers #js {"Accept" transit-type}})
        (.then (fn [response]
                 (if (.-ok response)
                   (.text response)
                   (throw (js/Error. "filters request failed")))))
        (.then (fn [body]
                 (let [options (read-transit body)]
                   (swap! state
                          (fn [current]
                            (cond-> (assoc current :filters-pending? false)
                              (= corpora (chosen-corpora current))
                              (-> (assoc :filters-for corpora)
                                  (update :filter-controls merge
                                          (select-keys options
                                                       [:attrs :unlisted])))))))))
        (.catch (fn [_] (swap! state assoc :filters-pending? false))))))

(defn refresh-filters!
  "Fetch the metadata filters the selection now offers, once it has held
  still for `filters-debounce-ms`; a no-op while what is on screen already
  describes the selection, or while nobody is looking at it."
  []
  (debounce! filters-timer filters-debounce-ms
             (fn []
               (when (filters-stale? @state)
                 (fetch-filters! (chosen-corpora @state))))))

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
  "Close the inspection panel once focus has settled outside both the
  concordance and the panel, which are one pool: focus moving between
  them keeps the panel and focus leaving either for the page closes it,
  so both report focus leaving, and which of them did does not matter.

  Deferred by a tick because focusout fires before the next element has
  focus, and read from `activeElement` rather than the event's
  relatedTarget so that clicking the page background closes the panel
  while merely switching windows does not: a blurred window keeps its
  active element, an abandoned concordance does not."
  []
  (js/setTimeout
   (fn []
     (let [el     (.-activeElement js/document)
           region (.getElementById js/document kwic/region-id)
           panel  (.querySelector js/document "aside.sidebar")]
       (when-not (or (and region (.contains region el))
                     (and panel (.contains panel el)))
         (swap! state dissoc :selected))))
   0))

(defn own-rows
  "The tokens of an extended search as this client shows them: the served
  `tokens` (see dk.cst.corpus-probe.query/form-rows) less the blank
  last one the server ends them in for a reader without a client, who
  has no button to add one. Kept when it is the only one."
  [tokens]
  (if (and (next tokens) (not (url/asks? (last tokens))))
    (vec (butlast tokens))
    tokens))

(defn focus-field!
  "Move focus to the form control named `name`, once the render that put
  it there has run: a reader who added or took away a token or a
  condition is left on what is now there rather than on the body, which
  is where focus falls when the button under it goes."
  [name]
  (some-> (.querySelector js/document (str "[name=\"" name "\"]"))
          (.focus)))

(defn place
  "Where the item with `id` stands among `items`, counted from one."
  [items id]
  (inc (count (take-while #(not= id (:id %)) items))))

(defn without
  "`items` less the one with `id`; `fallback` alone when that was the
  last, since neither the tokens nor a token's conditions may run out."
  [items id fallback]
  (let [left (vec (remove #(= id (:id %)) items))]
    (if (seq left) left [fallback])))

(defn add-token!
  "Add a blank token after the last and focus its attribute."
  []
  (swap! state update :tokens
         (fn [tokens]
           (conj (vec tokens)
                 (url/blank-token (inc (reduce max 0 (map :id tokens)))))))
  (focus-field! (url/token-key (count (:tokens @state)) 1 :attr)))

(defn remove-token!
  "Take the token with `id` away and focus the attribute of the token now
  in its place, or of the last."
  [id]
  (let [k (place (:tokens @state) id)]
    (swap! state update :tokens without id (url/blank-token (inc id)))
    (focus-field! (url/token-key (min k (count (:tokens @state))) 1 :attr))))

(defn add-condition!
  "Add a blank condition to token `i`, counted from one, and focus its
  join."
  [i]
  (swap! state update-in [:tokens (dec i) :conditions]
         (fn [conditions]
           (conj (vec conditions)
                 {:id (inc (reduce max 0 (map :id conditions)))})))
  (focus-field! (url/token-key i (count (get-in @state [:tokens (dec i)
                                                        :conditions]))
                               :join)))

(defn remove-condition!
  "Take the condition with `id` away from token `i`, counted from one,
  and focus the condition now in its place, or the last: its join, or
  the attribute of a first condition, which has none."
  [i id]
  (let [path [:tokens (dec i) :conditions]
        k    (place (get-in @state path) id)]
    (swap! state update-in path without id {:id (inc id)})
    (let [c (min k (count (get-in @state path)))]
      (focus-field! (url/token-key i c (if (= 1 c) :attr :join))))))

(defn switch-mode!
  "Change the query mode of `form` (the form element) to `mode`, holding
  in the new mode's form as much of the query the old one holds as it
  can, as the server does for a submitted form (see
  dk.cst.corpus-probe.query/project and /loss), and saying the rest in
  the form's status line.

  The old form is read as it stands, not as it was served, so a word
  typed or an option changed since the last search comes along. What
  its mode reads is replaced by the new mode's spelling of what it
  holds (see dk.cst.corpus-probe.query/params); what neither mode reads
  stays in the params as memory, for the disabled control that shows it.

  Switching away and back loses nothing while nothing was edited: the
  query the last switch started from is `:remembered`, and the form it
  handed the reader `:projected`; a form still holding that projection
  is read as the remembered query, so that Simple, CQP and back is the
  words again and not the CQP read as words, and Extended, Simple and
  back is every token again.

  The result on screen is left as it is: it answers what was asked (see
  dk.cst.corpus-probe.views.page/result-section), and the form has
  moved on."
  [form mode]
  (swap! state
         (fn [{:keys [params remembered projected] :as s}]
           (let [from    (url/mode params)
                 live    (form-params form)
                 typed   (query/of (assoc live :mode from))
                 source  (if (= typed projected) remembered typed)
                 held    (query/project mode source)
                 memory  (-> (apply dissoc params (url/read-keys from params))
                             (merge (select-keys live
                                                 (filter url/query-key?
                                                         (keys live)))))]
             (assoc s
                    :params     (-> (apply dissoc memory :from
                                           (url/read-keys mode memory))
                                    (merge (query/params mode held))
                                    (assoc :mode mode))
                    :tokens     (own-rows (query/form-rows
                                           (when (= "extended" mode) held)))
                    :switch     {:loss (query/loss mode source) :unread #{}}
                    :remembered source
                    :projected  held)))))

(defn handle!
  "Apply an `action` to the state, using `data` (the Replicant dispatch
  data) for the key a token was pressed with.

  `:inspect` fires on focus as well as on click, so the panel describes
  whatever the cursor is on rather than waiting for a press.
  `:move-cursor` answers a key pressed on a token, `:leave-concordance`
  closes the panel once focus has gone elsewhere, and `:close` dismisses
  it from its own button. `:toggle-context` expands a hit (fetching its
  wider context) or collapses it. `:set-mode` changes the query mode
  the reader picked the form to, carrying the query across as far as
  the new mode holds it and saying the rest (see `switch-mode!`).
  `:apply-view` submits the search again with a result control as it now
  stands, so choosing an order is asking for it. `:add-token` and
  `:remove-token` add a token to the extended search and take one away,
  `:add-condition` and `:remove-condition` do the same to a token's
  conditions (see `add-token!` and the others, which also move focus),
  and `:set-condition` records a condition's operator or attribute,
  which decide which of its controls are live and which values its
  field suggests; the values typed stay in the DOM, which a re-render
  leaves alone, since tokens and conditions are keyed by their ids.
  `:toggle-corpora` records a corpus box or a whole folder being selected
  or cleared, and `:set-checkbox-state`, a render hook rather than an
  event, writes the states of a checkbox that no attribute carries: partly
  checked, for a folder holding only part of the selection, and invalid,
  for the corpus chooser while nothing is chosen.
  `:toggle-filter-values` records metadata values being chosen or dropped,
  one or a whole attribute at a time, so the filter counts what the boxes
  say rather than what the last search asked, and `:clear-filter` empties
  it. `:filter-values` records what the reader is looking for among those
  values and `:swallow-enter` keeps Enter in either filter box from
  submitting the search. `:set-filters-open` records the metadata filter
  being opened or shut, which both decides the disclosure and says whether
  the filters it holds are worth fetching again. `:set-chooser-open`
  records the corpus tree being opened or shut, so that a filter can open
  it without emptying the box shutting it again. `:filter-corpora` records
  what the reader is looking for in the corpus chooser.

  Re-rendering the form does not disturb what the reader has typed: the
  query input's value is the same in both renders, so Replicant leaves the
  element alone."
  [data [action arg]]
  (case action
    ;; the field changes shape between the modes (a list is a text
    ;; area), so what was typed is carried into the new element rather
    ;; than left behind in the old one; the extended mode has no field,
    ;; so leaving it keeps the query the state already holds, and
    ;; entering it with nothing asked yet seeds the tokens from the query
    :set-mode
    (switch-mode! (.-form (.-target (:replicant/dom-event data))) arg)
    :add-token        (add-token!)
    :remove-token     (remove-token! arg)
    :add-condition    (add-condition! arg)
    :remove-condition (let [[i id] arg] (remove-condition! i id))
    :set-condition
    (let [[i id field] arg
          value (.-value (.-target (:replicant/dom-event data)))]
      (swap! state update-in [:tokens (dec i) :conditions]
             (fn [conditions]
               (mapv #(cond-> % (= id (:id %)) (assoc field value))
                     conditions))))
    :apply-view (apply-view!)
    :toggle-corpora (do (toggle-corpora! arg) (refresh-filters!))
    ;; what metadata a selection offers is the server's to say, so it is
    ;; fetched rather than known, and only once a reader looks at it
    :set-filters-open
    (do (swap! state assoc :filters-open?
               (.-open (.-target (:replicant/dom-event data))))
        (refresh-filters!))
    :toggle-filter-values
    (swap! state update-in [:filter-controls :selected]
           choose-values (first arg) (second arg))
    :clear-filter
    (swap! state assoc-in [:filter-controls :selected] {})
    :filter-values (swap! state assoc :value-filter
                          (.-value (.-target (:replicant/dom-event data))))
    ;; the reader owns the corpus tree's disclosure once they have opened
    ;; or shut it; before that the view decides from what was served
    :set-chooser-open
    (swap! state assoc :chooser-open?
           (.-open (.-target (:replicant/dom-event data))))
    :filter-corpora
    (swap! state assoc :corpus-filter
           (.-value (.-target (:replicant/dom-event data))))
    ;; a text field in a form submits it on Enter, and a reader finding
    ;; something to tick is not asking for an answer yet
    :swallow-enter
    (let [event (:replicant/dom-event data)]
      (when (= "Enter" (.-key event))
        (.preventDefault event)))
    ;; a checkbox has three states and only two of them are attributes,
    ;; and the constraint of a group of them is no attribute of any one
    ;; box, so both are written to the element itself on every render
    :set-checkbox-state
    (let [node (:replicant/node data)]
      (set! (.-indeterminate node) (:indeterminate arg))
      (.setCustomValidity node (or (:invalid arg) "")))
    :inspect (swap! state assoc :selected arg)
    :close   (close-panel!)
    :move-cursor (move-cursor! (:replicant/dom-event data) arg)
    :leave-concordance (leave-concordance!)
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
  new entries; a no-op when the URL already says that.

  The whole query string is rewritten rather than one param set on it,
  so the URL in the bar is the canonical one whatever was typed:
  `mode=simple` goes and the corpora become one param. Only on the search
  page, whose query string is the one that rule knows: on any other
  page it would drop what it does not know, and the reading page names
  its position that way. Whatever the page, what is on screen is
  recorded (see `shown`), so a fragment jump is not taken for a page to
  fetch."
  []
  ;; a switch URL, carrying the query of the mode the form was in, is
  ;; left as it is: the rule would drop that query, and a reload would
  ;; then find an empty form (see dk.cst.corpus-probe.query/arrived)
  (when (and (= url/search js/location.pathname)
             (not (url/unread-query? (location-params))))
    (let [url    (current-url)
          ks     (sort (keys (:expanded @state)))
          params (cond-> (dissoc (location-params) :expand)
                   (seq ks)
                   (assoc :expand (str/join "," (map (fn [[corpus cpos]]
                                                       (str corpus ":" cpos))
                                                     ks))))]
      (set! (.-search url) (url/query-string params))
      ;; only when it would say something new: this runs on every render,
      ;; and a render happens on every arrow key. Safari throws past
      ;; roughly a hundred history writes in thirty seconds, and that
      ;; throw comes back out through the watcher into the swap! that
      ;; moved the cursor
      (when (not= (.-href url) js/location.href)
        (.replaceState js/history nil "" (.-href url)))))
  (reset! shown (page-key)))

(defn render!
  "Render the current state into the masthead, #app and the footer, then
  sync the URL to it."
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
    ;; the document's own language, which only the server sets: a routed
    ;; change of it would otherwise leave the page saying it is in the
    ;; language it was served in while every word on it is in another
    (set! (.-lang (.-documentElement js/document)) lang)
    ;; the masthead's links carry the current search, so it re-renders with
    ;; the page rather than keeping whatever the first server render said
    (r/render (.getElementById js/document "masthead")
              (layout/site-header (i18n/->ui lang) path nav))
    (r/render (.getElementById js/document "app") (app-views/page state))
    ;; the footer's words are in the UI language, so a language switch
    ;; that does not reload the document has to re-render it too, or it
    ;; keeps the language the page was served in
    (r/render (.getElementById js/document "footer")
              (layout/site-footer (i18n/->ui lang))))
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

(defn cited-href
  "`href` as the history should hold it: without the results fragment,
  which tells a browser where to land and a reader nothing, and which
  the client lands without. Any other fragment is a place in the page
  and stays."
  [href]
  (let [url (js/URL. href js/location.href)]
    (when (= url/results-fragment (.-hash url))
      (set! (.-hash url) ""))
    (.-href url)))

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

(defn client-state
  "Server `data` as the state this client renders from: marked as the
  client's, seeded with the expansions the URL names, and remembering the
  corpora the page was served for.

  Those last two are what the corpus chooser and the metadata filter judge
  what to open by. The selections themselves change under the reader as
  they tick boxes, and a disclosure that opened and shut with them would
  fight them; what the page arrived with holds still until the next page
  does."
  [data]
  (-> data
      (assoc :client?       true
             ;; the place in the page the location names, which a
             ;; document marks (see dk.cst.corpus-probe.views.app/mark-target)
             :fragment      (fragment)
             :served-corpus (get-in data [:params :corpus])
             :served-filter (get-in data [:filter-controls :selected])
             ;; what the filters on screen describe, so that a selection
             ;; that has changed can be told from one that has not
             :filters-for   (vec (sort (get-in data [:params :corpus]))))
      (update :tokens own-rows)
      (with-expansions)))

(defn fetch-expansions!
  "Fetch the wider context of every hit currently placeheld in `:expanded`."
  []
  (doseq [[[corpus cpos] v] (:expanded @state)
          :when (= ::loading v)
          :let [hit (first (filter #(= [corpus cpos] (kwic/hit-key %))
                                   (hits)))]
          :when hit]
    (fetch-context! corpus cpos (:matchend (:anchors hit)))))

(defn fetch-counts!
  "Fetch the count of the search on screen while its corpora are still
  being counted (see dk.cst.corpus-probe.search/concordance!), and put it
  in the state: the counts, the size and the number of pages of the
  result, the page links and the document title, all of which the count
  decides.

  Asked with the page's own query string, so the server counts the
  question the page answered. Applied only while that page is still the
  one on screen, so a count arriving after the reader has moved on is
  dropped rather than written over the answer to their next question. A
  count that fails falls back to a real navigation, as a page that fails
  does: the server renders the page with its count in full."
  []
  (when (seq (get-in @state [:result :remaining]))
    (let [key (page-key)]
      (-> (js/fetch (str "/api/counts" js/location.search)
                    #js {:headers #js {"Accept" transit-type}})
          (.then (fn [response]
                   (if (.-ok response)
                     (.text response)
                     (throw (js/Error. "counts request failed")))))
          (.then (fn [body]
                   (when (= key @shown)
                     (let [counted (read-transit body)]
                       (set! (.-title js/document) (:title counted))
                       (swap! state
                              (fn [s]
                                (-> s
                                    (update :result
                                            #(-> (merge % (select-keys
                                                           counted
                                                           [:counts :size
                                                            :pages]))
                                                 (dissoc :remaining)))
                                    (merge (select-keys counted
                                                        [:prev-href
                                                         :next-href])))))))))
          (.catch (fn [_]
                    (when (= key @shown)
                      (set! (.-href js/location) js/location.href))))))))

(defn landed-href
  "The address a routed navigation to `href` answered by `response` lands
  on: where the response came from, which is elsewhere after a redirect,
  with the fragment of `href`, which a fetch never sends and a browser
  keeps across a redirect. `href` itself when the response names no
  address."
  [href response]
  (if (seq (.-url response))
    (let [url (js/URL. (.-url response))]
      (set! (.-hash url) (.-hash (js/URL. href js/location.href)))
      (.-href url))
    href))

(defn navigate!
  "Fetch the route at `href` as data and render it, without a page load.

  Falls back to a real navigation on any failure, so a route the client
  cannot render is still a working page: the server renders every one of
  them. `push?` adds a history entry; a popstate replaces nothing.

  Marks the state pending once an answer is `pending-delay-ms` late, and
  not before. A whole-corpus search can run for minutes, and until the
  client routed anything the browser reported that wait itself; an answer
  that arrives at once wants no report at all. The answer replaces the
  state wholesale, which is how the mark is cleared.

  A navigation started while one is in flight calls the first off rather
  than racing it, so the reader gets the answer to their latest question,
  and the abandoned one does not mistake being called off for failing and
  load the page the reader has already left.

  Any expansion the URL names is restored, so going back to a page whose
  hits were expanded shows them expanded again. What is pushed is the
  `cited-href`: the address of the result, not the instruction to land
  on it."
  [href push?]
  (let [controller (js/AbortController.)]
    (some-> @in-flight (.abort))
    (reset! in-flight controller)
    (debounce! pending-timer pending-delay-ms
               #(swap! state assoc :pending? true))
    (-> (js/fetch href #js {:headers #js {"Accept" transit-type}
                            :signal  (.-signal controller)})
        (.then (fn [response]
                 (if (.-ok response)
                   ;; the address the answer came from travels with it:
                   ;; a redirect may have moved it (see `landed-href`)
                   (.then (.text response)
                          (fn [body] [(landed-href href response) body]))
                   (throw (js/Error. "route request failed")))))
        (.then (fn [[landed body]]
                 (let [data (read-transit body)]
                   ;; before the state is replaced, or a report scheduled
                   ;; for a wait that is over lands on the answer to it
                   (cancel! pending-timer)
                   (when push?
                     (.pushState js/history nil "" (cited-href landed)))
                   (reset! shown (page-key))
                   (set! (.-title js/document) (:title data))
                   (reset! state (client-state data))
                   (fetch-expansions!)
                   (fetch-counts!)
                   (land!))))
        (.catch (fn [_]
                  ;; an abort leaves the timer alone: it belongs to the
                  ;; navigation that did the aborting, which is still in
                  ;; flight and may yet be worth reporting
                  (when-not (.-aborted (.-signal controller))
                    (cancel! pending-timer)
                    (set! (.-href js/location) href)))))))

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
  #{url/home url/search url/corpora url/glossary})

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
  "True when `el` is a link this app renders itself: a page it knows, same
  origin, not a place in this one, no target and no modifier, so the
  browser's own meaning of the click is kept."
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
           (let [url (js/URL. (.-action form))]
             (set! (.-search url) (form-query form))
             (navigate! (.-href url) true)))))))
  ;; a fragment navigation fires this too, and the page it is within is
  ;; already on screen: the browser has moved to the place itself, as it
  ;; does for the bypass link, and fetching the page again would only
  ;; take the reader somewhere else
  (.addEventListener js/window "popstate"
                     (fn [_]
                       (when (not= (page-key) @shown)
                         (navigate! js/location.href false))))
  ;; a link within the page, or back and forth between two places in
  ;; it, changes only what the page marks
  (.addEventListener js/window "hashchange"
                     (fn [_] (swap! state assoc :fragment (fragment)))))

(defn ^:dev/after-load reload!
  "Render again once shadow-cljs has swapped in recompiled code.

  Nothing else would: the views are called from the state watcher, and a
  recompile changes the code without changing the state, so a saved file
  would sit invisible until the reader next did something. State itself
  survives, which is the point of a watch: the search stays on screen
  while the view that draws it is edited."
  []
  (render!))

(defn init!
  "Boot the client: seed state from the bootstrap payload, wire dispatch and
  routing, re-render on every state change, and restore any expansions the
  URL names, exactly as a routed navigation does."
  []
  ;; the views render tokens and disclosures as controls only where this
  ;; script is running to answer them
  (reset! state (client-state (read-payload)))
  (route-clicks!)
  (r/set-dispatch! handle!)
  (add-watch state ::render (fn [_ _ _ _] (render!)))
  (render!)
  (fetch-expansions!))
