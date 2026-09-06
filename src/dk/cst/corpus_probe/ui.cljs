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
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.corpus :as corpus-views]
            [dk.cst.corpus-probe.views.kwic :as kwic]
            [dk.cst.corpus-probe.views.layout :as layout]
            [dk.cst.corpus-probe.views.page :as page]
            [dk.cst.corpus-probe.views.tree :as tree]
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

(def lists
  "The two lists a reader chooses from, the corpus chooser and the
  metadata filter, by the name their controls send, each with what the
  client needs to keep its state apart from the rest of the page.

  That state is under `:lists` in the state, per list: `:open`, the set
  of its disclosures standing open, `:root` for its own and the id of
  each node for theirs, which the document mirrors exactly (see
  `toggle-open!`); `:choosing?`, whether the reader is choosing from it,
  which shows everything in it rather than only what is chosen (see
  `engage!`); and `:unticked`, what they have unticked at rest since
  they last left, which stays in place until then (see `tick`). Both
  lists are one chooser (see dk.cst.corpus-probe.views.tree/chooser),
  and its rules are applied to that state here.

  Here, per list: `:tree` builds its tree from the state, with what is
  held chosen kept in it, and `:chosen` reads its selection out of the
  state as the set of leaf ids the tree names."
  {:corpora {:tree   (fn [s _]
                       (corpus-views/tree (i18n/->ui (:lang s)) (:folders s)))
             :chosen (fn [s] (set (get-in s [:params :corpus])))}
   :values  {:tree   (fn [s held] (page/tree (:filter-controls s) held))
             :chosen (fn [s]
                       (page/pairs (get-in s [:filter-controls :selected])))}})

(defn held
  "What the resting view of list `k` (see `lists`) treats as chosen in
  state `s`: its selection, and what was unticked at rest since the
  reader last left."
  [s k]
  (into ((:chosen (lists k)) s) (get-in s [:lists k :unticked])))

(defn rest-open
  "The disclosures of list `k` (see `lists`) that stand open at rest in
  state `s` with `held` chosen (see
  dk.cst.corpus-probe.views.tree/open-at-rest)."
  [s k held]
  (tree/open-at-rest ((:tree (lists k)) s held) held))

(defn settle
  "State `s` with list `k` (see `lists`) at rest: nobody choosing from
  it, nothing unticked, and open exactly what the resting view opens
  over what is chosen now, except the root, which stays shut if the
  reader shut it: a reader who folded the list up has said so."
  [s k]
  (let [resting (rest-open s k ((:chosen (lists k)) s))]
    (update-in s [:lists k] assoc
               :choosing? false
               :unticked  #{}
               :open      (cond-> resting
                            (not (contains? (get-in s [:lists k :open]) :root))
                            (disj :root)))))

(defn tick
  "State `s` once the `ids` of list `k` (see `lists`) have been ticked
  or, when `unticking?`, unticked.

  At rest, what is unticked stays in place until the reader leaves (see
  `held`), so that a box unticked by mistake is there to be ticked
  again, and only what the change leaves chosen whole or not at all
  shuts, which is never anything the reader is looking into. While they
  are choosing, nothing moves at all."
  [s k ids unticking?]
  (let [{:keys [choosing? open]} (get-in s [:lists k])
        s (update-in s [:lists k :unticked]
                     (if unticking? into #(apply disj % ids)) ids)]
    (cond-> s
      (not choosing?)
      (assoc-in [:lists k :open]
                (set/intersection open (rest-open s k (held s k)))))))

(defn engage!
  "Take the reader to be choosing from list `k` (see `lists`): nothing
  in it is hidden from here until they leave, and its root stands open,
  since the box that asks for this sits in the root's summary and is
  reached whether the root is open or shut."
  [k]
  (swap! state update-in [:lists k]
         #(-> % (assoc :choosing? true) (update :open conj :root))))

(defn leave!
  "Put list `k` (see `lists`) at rest, the reader having gone elsewhere
  (see `settle`), unless its filter box still holds something: a filter
  in force is a reader still looking, and the list stays as the filter
  left it. Nothing to do for a list already at rest with nothing
  unticked, which is most lists most of the time."
  [k]
  (let [{:keys [choosing? unticked] q :filter} (get-in @state [:lists k])]
    (when (and (str/blank? q) (or choosing? (seq unticked)))
      (swap! state settle k))))

(defn toggle-open!
  "Record disclosure `id` of list `k` (see `lists`) as `open?`, the reader
  having worked it, and answer what that says: a disclosure coming open
  while nobody is choosing is the reader asking to choose (see
  `engage!`), and the root shutting is them finishing (see `leave!`).

  A <details> fires its own toggle when this client opens or shuts it
  too. That echo says what the state already says, so it is ignored, and
  only a toggle that differs from the state is the reader's: the state
  is what the document was rendered from, so the two differ only where a
  reader has worked the disclosure since."
  [k id open?]
  (let [{:keys [open choosing?]} (get-in @state [:lists k])]
    (when-not (= open? (contains? open id))
      (swap! state update-in [:lists k :open] (if open? conj disj) id)
      (cond
        (and open? (not choosing?)) (engage! k)
        (and (not open?) (= :root id)) (leave! k)))))

(defn filter!
  "Narrow list `k` (see `lists`) to whatever answers `q`, and open every
  disclosure holding something that does: a reader who has asked where
  something is has asked to be shown it."
  [k q]
  (swap! state (fn [s]
                 (cond-> (assoc-in s [:lists k :filter] q)
                   (not (str/blank? q))
                   (update-in [:lists k :open] into
                              (tree/matching q ((:tree (lists k)) s
                                                (held s k))))))))

(defn toggle-corpora!
  "Select every corpus in `ids`, or clear them all when they are already
  selected, and note the change for the chooser (see `tick`).

  One rule serves a single corpus and a whole folder alike: a box that is
  on turns off, and a folder that is wholly selected clears, while a
  folder that is only partly selected fills rather than clearing the part
  of it the reader already had."
  [ids]
  (swap! state
         (fn [s]
           (let [corpus     (get-in s [:params :corpus])
                 unticking? (every? (set corpus) ids)]
             (-> s
                 (assoc-in [:params :corpus]
                           (select-corpora corpus ids (not unticking?)))
                 (tick :corpora ids unticking?))))))

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

  Only while the filter stands open: filters nobody has opened are
  filters nobody has to fetch, and a corpus selection is usually changed
  several times before anyone asks what metadata it carries. Unless
  nothing is on show at all (see
  dk.cst.corpus-probe.views.page/filterable?), since a fieldset that is
  not there is one the reader cannot open to ask. And never for no
  corpora: a search cannot run without one, and what the server answers
  for none is nothing, which would only take the fieldset away."
  [{:keys [filters-for filter-controls] :as state}]
  (and (or (contains? (get-in state [:lists :values :open]) :root)
           (not (page/filterable? filter-controls)))
       (seq (chosen-corpora state))
       (not= (chosen-corpora state) filters-for)))

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
                                                       [:attrs :unlisted]))
                                  ;; new attributes, so what the resting
                                  ;; view opens is decided afresh, unless
                                  ;; the reader is in the list by now
                                  (cond->
                                    (not (get-in current
                                                 [:lists :values :choosing?]))
                                    (settle :values)))))))))
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

(defn control-value
  "What the form control an `event` came from now holds, as its param
  would carry it: its value, or, for a checkbox, `on` when ticked and
  nil when not, which is the absence the form submits."
  [event]
  (let [el (.-target event)]
    (if (= "checkbox" (.-type el))
      (when (.-checked el) "on")
      (.-value el))))

(defn with-field
  "`m`, a token or a condition as the form holds it, with `field` set to
  `value`, or without the field for a nil value: a checkbox unticked is
  a field the form does not submit."
  [m field value]
  (if (some? value) (assoc m field value) (dissoc m field)))

(defn switch-mode!
  "Change the form of the query in `form` (the form element) to `mode`
  (see dk.cst.corpus-probe.url/forms), holding in the new form as much
  of the query the old one holds as it can, as the server does for a
  submitted form (see dk.cst.corpus-probe.query/project and /loss), and
  saying the rest in the form's status line.

  The old form is read as it stands, not as it was served, so a word
  typed or an option changed since the last search comes along. The
  field's text seeds the tokens, read by its shape, and the tokens are
  handed to the field as CQP. What the old form reads is replaced by
  the new form's spelling of what it holds (see
  dk.cst.corpus-probe.query/params); what neither reads stays in the
  params as memory.

  Switching away and back loses nothing while nothing was edited: what
  the form the last switch left held is `:remembered`, its params and
  its token rows, and the query the switch handed the reader
  `:projected`; a form still holding that projection gets the
  remembered form back as it was, so that the words a reader typed come
  back as those words and not as the CQP the tokens are, and every
  token comes back from the field.

  The result on screen is left as it is: it answers what was asked (see
  dk.cst.corpus-probe.views.page/result-section), and the form has
  moved on."
  [form mode]
  (swap! state
         (fn [{:keys [params tokens remembered projected] :as s}]
           (let [from    (url/form params)
                 live    (form-params form)
                 typed   (query/of (assoc live :mode from))
                 back?   (and (= typed projected) (= mode (:form remembered)))
                 target  (if (= "extended" mode) mode "cqp")
                 held    (when-not back? (query/project target typed))
                 spelt   (if back?
                           (:params remembered)
                           (query/params target held))
                 memory  (-> (apply dissoc params (url/read-keys from params))
                             (merge (select-keys live
                                                 (filter url/query-key?
                                                         (keys live)))))]
             (assoc s
                    :params     (-> (apply dissoc memory
                                           (url/read-keys mode memory))
                                    (merge spelt)
                                    (assoc :mode mode))
                    :tokens     (cond
                                  (not= "extended" mode)
                                  (own-rows (query/form-rows nil))
                                  back? (:tokens remembered)
                                  :else (own-rows (query/form-rows held)))
                    :switch     {:loss   (if back? [] (query/loss target typed))
                                 :unread #{}}
                    :remembered {:form   from
                                 :params (select-keys live
                                                      (url/read-keys from live))
                                 :tokens tokens}
                    :projected  (query/of (assoc spelt :mode mode)))))))

(defn handle!
  "Apply an `action` to the state, using `data` (the Replicant dispatch
  data) for the key a token was pressed with.

  `:inspect` fires on focus as well as on click, so the panel describes
  whatever the cursor is on rather than waiting for a press.
  `:move-cursor` answers a key pressed on a token, `:leave-concordance`
  closes the panel once focus has gone elsewhere, and `:close` dismisses
  it from its own button. `:toggle-context` expands a hit (fetching its
  wider context) or collapses it. `:set-mode` changes the form of the
  query to the one the reader picked, carrying the query across as far
  as the new form holds it and saying the rest (see `switch-mode!`);
  `:submit-on-enter` makes Enter in the field a submit.
  `:apply-view` submits the search again with a result control as it now
  stands, so choosing an order is asking for it. `:add-token` and
  `:remove-token` add a token to the extended search and take one away,
  `:add-condition` and `:remove-condition` do the same to a token's
  conditions (see `add-token!` and the others, which also move focus),
  `:set-condition` and `:set-token` record a condition's or a token's
  field as the reader sets it, so the state holds the tokens as typed
  and the CQP line under them follows (see
  dk.cst.corpus-probe.views.page/cqp-line), a checkbox as its `on` or
  nothing; `:set-query` records the query field likewise, so the answer
  can tell when the form has moved on from what ran (see
  dk.cst.corpus-probe.views.page/question).
  `:toggle-corpora` records a corpus box or a whole folder being selected
  or cleared, `:toggle-filter-values` metadata values being chosen or
  dropped, one or a whole attribute at a time, and `:clear-filter` the
  whole filter being emptied, so that each list counts what its boxes
  say rather than what the last search asked (see `tick`).
  `:set-validity`, a render hook, writes a control's own constraint,
  which the query field uses to refuse a blank of any length;
  `:set-checkbox-state`, a render hook rather than an event, writes the
  states of a checkbox that no attribute carries: partly checked, for a
  folder holding only part of the selection, and invalid, for the corpus
  chooser while nothing is chosen.
  The rest work the two lists (see `lists`), named by the action's
  first argument: `:engage` is the filter box taking focus, `:filter`
  what is typed in it, `:toggle-open` one of the list's disclosures
  opening or shutting, named by the second argument, `:leave` focus
  leaving the fieldset (a click elsewhere is heard by `leave-on-click!`),
  and `:swallow-enter` keeps Enter in either box from submitting the
  search.

  Re-rendering the form does not disturb what the reader has typed: the
  query input's value is the same in both renders, so Replicant leaves the
  element alone."
  [data [action arg id]]
  (case action
    :set-mode
    (switch-mode! (.-form (.-target (:replicant/dom-event data))) arg)
    :add-token        (add-token!)
    :remove-token     (remove-token! arg)
    :add-condition    (add-condition! arg)
    :remove-condition (let [[i id] arg] (remove-condition! i id))
    :set-query
    (swap! state assoc-in [:params :q]
           (.-value (.-target (:replicant/dom-event data))))
    ;; the field is a text area, so that a list can be typed one word per
    ;; line, and a text area takes Enter as a line; a search box takes it
    ;; as a submit, which is what a reader pressing it after a word
    ;; expects. So Enter submits and a line is Shift+Enter, as the chat
    ;; boxes have it; not while an input method is composing, when Enter
    ;; commits the composition
    :submit-on-enter
    (let [event (:replicant/dom-event data)]
      (when (and (= "Enter" (.-key event))
                 (not (.-shiftKey event))
                 (not (.-isComposing event)))
        (.preventDefault event)
        (.requestSubmit (.-form (.-target event)))))
    :set-condition
    (let [[i id field] arg
          value (control-value (:replicant/dom-event data))]
      (swap! state update-in [:tokens (dec i) :conditions]
             (fn [conditions]
               (mapv #(if (= id (:id %)) (with-field % field value) %)
                     conditions))))
    :set-token
    (let [[i field] arg
          value (control-value (:replicant/dom-event data))]
      (swap! state update-in [:tokens (dec i)] with-field field value))
    :apply-view (apply-view!)
    :toggle-corpora (do (toggle-corpora! arg) (refresh-filters!))
    :toggle-filter-values
    (let [[attr values] arg]
      (swap! state
             (fn [s]
               (let [chosen (set (get-in s [:filter-controls :selected attr]))]
                 (-> s
                     (update-in [:filter-controls :selected]
                                choose-values attr values)
                     (tick :values (map (partial vector attr) values)
                           (every? chosen values)))))))
    :clear-filter
    (swap! state
           (fn [s]
             (-> s
                 (assoc-in [:filter-controls :selected] {})
                 (tick :values (page/pairs (get-in s [:filter-controls :selected]))
                       true))))
    ;; what metadata a selection offers is the server's to say, so it is
    ;; fetched rather than known, and only once a reader looks at it
    :engage
    (do (engage! arg)
        (when (= :values arg) (refresh-filters!)))
    :toggle-open
    (do (toggle-open! arg id (.-open (.-target (:replicant/dom-event data))))
        (when (= :values arg) (refresh-filters!)))
    :filter
    (filter! arg (.-value (.-target (:replicant/dom-event data))))
    :leave
    (let [event (:replicant/dom-event data)
          to    (.-relatedTarget event)]
      ;; focus moving from one control of a fieldset to another is not
      ;; leaving it. Nor is focus landing on something outside the tab
      ;; order: a press on a label inside the fieldset sends focus to the
      ;; nearest focusable ancestor for a moment, which is <main> with
      ;; its tabindex of -1 (measured in Chrome), before the box it is
      ;; for takes it. Only the keyboard reaches an element in the tab
      ;; order, and a press elsewhere is heard by `leave-on-click!`
      (when (and to
                 (<= 0 (.-tabIndex to))
                 (not (.contains (.-currentTarget event) to)))
        (leave! arg)))
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
    ;; a constraint the markup cannot state, written to the control on
    ;; every render: the message it reports, or nothing
    :set-validity
    (.setCustomValidity (:replicant/node data) (or arg ""))
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
  client's, seeded with the expansions the URL names, and with each list
  at rest.

  The lists start at rest: what the chooser and the metadata filter show
  of a served page is what the search read, and nothing opens or shuts
  under the reader's hands from there (see `lists`)."
  [data]
  (-> data
      (assoc :client?       true
             ;; the place in the page the location names, which a
             ;; document marks (see dk.cst.corpus-probe.views.app/mark-target)
             :fragment      (fragment)
             :lists         (into {}
                                  (for [[k {:keys [tree chosen]}] lists
                                        :let [selected (chosen data)]]
                                    [k {:open      (tree/open-at-rest
                                                    (tree data selected)
                                                    selected)
                                        :choosing? false
                                        :unticked  #{}}]))
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

(defn leave-on-click!
  "Put a list at rest when the reader presses anywhere outside its
  fieldset (see `leave!`): the one gesture no element of the fieldset
  can hear, and the one a reader makes to go on to something else. A
  press rather than a click, so that a drag begun elsewhere counts, and
  a press rather than focus, since a label, a summary's words and the
  page around them take no focus, and on Safari neither does a box."
  []
  (.addEventListener
   js/document "pointerdown"
   (fn [e]
     (doseq [k (keys lists)]
       ;; the fieldset of a list is classed by its name (see
       ;; dk.cst.corpus-probe.views.tree/chooser)
       (when-not (some-> (.-target e) (.closest (str "." (name k))))
         (leave! k))))))

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
  (leave-on-click!)
  (r/set-dispatch! handle!)
  (add-watch state ::render (fn [_ _ _ _] (render!)))
  (render!)
  (fetch-expansions!))
