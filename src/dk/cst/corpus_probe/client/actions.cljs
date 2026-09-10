(ns dk.cst.corpus-probe.client.actions
  "The pure step of the client: `act` takes the state and an action and
  answers with the state to keep and the effects to run. Nothing here
  touches the document, a timer, the network or the history."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.client.lists :as lists]
            [dk.cst.corpus-probe.client.router :as router]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.tokens :as tokens]
            [dk.cst.corpus-probe.settings :as settings]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.chooser :as chooser]
            [dk.cst.corpus-probe.views.concordance :as concordance]
            [dk.cst.corpus-probe.views.search :as search-views]
            [dk.cst.corpus-probe.views.search.filter :as filter-views]))

(def arrow-keys
  "How each key moves the concordance's cursor: [rows tokens], a step
  between hits and a step within one."
  {"ArrowRight" [0 1]
   "ArrowLeft"  [0 -1]
   "ArrowDown"  [1 0]
   "ArrowUp"    [-1 0]})

(defn hits
  "The hits of the page `state` shows, in the order they are rendered."
  [state]
  (get-in state [:result :hits] []))

(defn hit-at
  "The index in `hits` of the hit the cursor `[k]` rests on."
  [hits [k]]
  (or (first (keep-indexed (fn [n hit] (when (= k (concordance/hit-key hit)) n))
                           hits))
      0))

(defn step-cursor
  "The cursor moved `[rows tokens]` through `hits`: along a row it stops
  at its ends rather than wrapping; between rows it keeps its distance
  from the match rather than its column, landing on the word the cursor
  was on, or its nearest token where the rows differ."
  [hits cursor [rows tokens]]
  (let [at   (hit-at hits cursor)
        here (nth hits at)
        i    (second cursor)]
    (if (zero? rows)
      [(concordance/hit-key here)
       (min (dec (concordance/token-count here)) (max 0 (+ i tokens)))]
      (let [there (nth hits (min (dec (count hits)) (max 0 (+ at rows))))]
        [(concordance/hit-key there)
         (concordance/offset->token there
                                    (concordance/token->offset here i))]))))

(def line-ends
  "The keys that go to the ends of a line, to the end each names: Home
  and End, and the Ctrl chords a text field answers the same way, which
  is what a reader with their hands on the keys reaches for."
  {"Home" :start "End" :end
   ["a" :ctrl] :start ["e" :ctrl] :end})

(defn line-end
  "Which end of the line key `pressed` asks for, `ctrl?` saying whether
  Ctrl was held (see `line-ends`); nil for a key that asks for neither."
  [pressed ctrl?]
  (get line-ends (if ctrl?
                   ;; a chord is the letter, whatever the shift key did to it
                   [(str/lower-case (str pressed)) :ctrl]
                   pressed)))

(defn widen
  "Ask for more of the line than the page holds, the reader having come
  to the end of `hit` going `tokens` along it: a fetch at twice the reach
  the page has now, with the step `owed` when the words arrive. Nothing
  to ask for at a `:bounds` of the hit, which is the end of its text."
  [state tokens hit owed]
  (let [{:keys [reach widest? widening]} (:result state)
        ended? (contains? (:bounds hit) (if (pos? tokens) :end :start))]
    (if (and (not (zero? tokens)) (not ended?) (number? reach) (not widest?)
             (not widening))
      ;; twice what the page holds, so that a reader who keeps going asks
      ;; a handful of times rather than once per word, and one who stops
      ;; after a step has paid for little more than they read
      {:state   (assoc-in state [:result :widening] owed)
       :effects [[:fetch-wider (* 2 reach)]]}
      {:state state})))

(defn running-out?
  "True when the cursor `[_ i]` stands near enough the end of `hit` in the
  direction `tokens` that the reader can see the line give out: the
  window shows `context` words past them and the fade a few more, so the
  words to ask for are wanted before the cursor arrives at the last one."
  [hit [_ i] tokens context]
  (and (number? context)
       (not (zero? tokens))
       (let [ahead (if (pos? tokens)
                     (- (concordance/token-count hit) 1 i)
                     i)]
         (<= ahead (+ context concordance/fade-steps)))))

(defn carried-cursor
  "Where `cursor` lands among the `wider` hits it had among `old`: the
  same word, which the context now to its left has moved along the row
  (see dk.cst.corpus-probe.views.concordance/token->offset), and then
  `tokens` further, the step the end of the line refused."
  [cursor old wider tokens]
  (let [[k i] cursor
        was   (concordance/hit-of old k)
        now   (concordance/hit-of wider k)]
    (if (and was now (< i (concordance/token-count was)))
      [k (concordance/offset->token
          now (+ tokens (concordance/token->offset was i)))]
      cursor)))

(defn wider-arrived
  "Put the hits of `data`, the page fetched to hold `reach` words either
  side, in `state`, with the cursor still on its word and the step it was
  refused taken (see `carried-cursor`). A page that came back no wider is
  all the line there is, and is not asked for again."
  [state reach data]
  (let [old    (get-in state [:result :hits])
        hits   (get-in data [:result :hits])
        length #(reduce + (map concordance/token-count %))
        rested #(dissoc % :widening)]
    (if (> (length hits) (length old))
      (let [cursor (carried-cursor (:cursor state) old hits
                                   (get-in state [:result :widening] 0))]
        {:state   (-> state
                      (assoc :cursor cursor)
                      (update :result #(rested (assoc % :hits hits
                                                        :reach reach))))
         :effects [[:focus (concordance/cursor-id cursor) true]]})
      {:state (update state :result #(rested (assoc % :widest? true)))})))

(defn move-cursor
  "Answer key `pressed` on the token at cursor `k` in `state`, `ctrl?`
  saying whether Ctrl was held: an arrow moves the cursor and focus with
  it, the `line-ends` keys go to the ends of the row the cursor is in,
  and Escape closes the panel; each consumed, since the concordance is
  one tab stop with a cursor inside it."
  [state k pressed ctrl?]
  (let [hits (hits state)
        end  (line-end pressed ctrl?)
        move (fn [cursor]
               {:state   (assoc state :cursor cursor)
                ;; the concordance travels to the token itself, so focus
                ;; must not jump the view there first
                :effects [[:prevent-default]
                          [:focus (concordance/cursor-id cursor) true]]})]
    (cond
      (= "Escape" pressed)
      {:state (dissoc state :selected) :effects [[:prevent-default]]}

      (contains? arrow-keys pressed)
      (let [step   (arrow-keys pressed)
            tokens (second step)
            cursor (step-cursor hits k step)
            hit    (nth hits (hit-at hits k))
            ;; a step that moves nothing has come to the end of the line;
            ;; one that lands near it will, so the words are asked for
            ;; while the reader is still reading their way there
            stuck? (= cursor k)
            asking (when (or stuck?
                             (running-out? hit cursor tokens
                                           (get-in state [:result :context])))
                     (widen state tokens hit (if stuck? tokens 0)))
            state* (:state asking state)]
        (if stuck?
          {:state state* :effects (into [[:prevent-default]] (:effects asking))}
          {:state   (assoc state* :cursor cursor)
           :effects (into [[:prevent-default]
                           [:focus (concordance/cursor-id cursor) true]]
                          (:effects asking))}))

      end
      (let [hit (nth hits (hit-at hits k))]
        (move [(concordance/hit-key hit)
               (if (= :start end) 0 (dec (concordance/token-count hit)))]))

      :else {:state state})))

(defn inspect
  "Put `selected`, the token the inspection panel describes, in `state`,
  or take it out for nil. The cursor follows it to `k`, so that the one
  tabbable token is the one the reader is on however they got there, and
  the concordance can keep it in the middle.

  Taking it out takes the cursor with it: the reader has left the
  concordance, and it goes back to resting on its matches rather than
  holding the line where they stopped reading."
  [state selected k]
  (if selected
    (cond-> (assoc state :selected selected)
      k (assoc :cursor k))
    (dissoc state :selected :cursor)))

(defn close
  "Dismiss the inspection panel of `state` from its own button and leave
  focus in the concordance it describes, rather than on a token."
  [state]
  {:state   (dissoc state :selected)
   ;; not a token, whose focus would open the panel again, and not the
   ;; button, which is about to go: the region, focusable since it
   ;; scrolls, from where a tab reaches the cursor again
   :effects [[:focus concordance/region-id]]})

(defn place
  "Where the item with `id` stands among `items`, counted from one."
  [items id]
  (inc (count (take-while #(not= id (:id %)) items))))

(defn without
  "Remove from `items` the one with `id`; `fallback` alone when that was the
  last, since neither the tokens nor a token's conditions may run out."
  [items id fallback]
  (let [left (vec (remove #(= id (:id %)) items))]
    (if (seq left) left [fallback])))

(defn add-token
  "Add to `state` a blank token after the last, and focus its attribute."
  [{:keys [tokens] :as state}]
  (let [id (inc (reduce max 0 (map :id tokens)))]
    {:state   (assoc state :tokens (conj (vec tokens) (tokens/blank-token id)))
     :effects [[:focus-field (tokens/token-key (inc (count tokens))
                                               1 :attr)]]}))

(defn remove-token
  "Remove from `state` the token with `id`, and focus the attribute of
  the token now in its place, or of the last."
  [{:keys [tokens] :as state} id]
  (let [k    (place tokens id)
        left (without tokens id (tokens/blank-token (inc id)))]
    {:state   (assoc state :tokens left)
     :effects [[:focus-field (tokens/token-key (min k (count left))
                                               1 :attr)]]}))

(defn add-condition
  "Add a blank condition to token `i` of `state`, counted from one, and
  focus its join."
  [state i]
  (let [path       [:tokens (dec i) :conditions]
        conditions (vec (get-in state path))
        id         (inc (reduce max 0 (map :id conditions)))]
    {:state   (assoc-in state path (conj conditions {:id id}))
     :effects [[:focus-field (tokens/token-key i (inc (count conditions))
                                               :join)]]}))

(defn remove-condition
  "Remove the condition with `id` from token `i` of `state`, counted from
  one, and focus the condition now in its place, or the last: its
  join, or the attribute of a first condition, which has none."
  [state i id]
  (let [path [:tokens (dec i) :conditions]
        k    (place (get-in state path) id)
        left (without (get-in state path) id {:id (inc id)})
        c    (min k (count left))]
    {:state   (assoc-in state path left)
     :effects [[:focus-field (tokens/token-key i c (if (= 1 c)
                                                     :attr
                                                     :join))]]}))

(defn with-field
  "Set `field` of `m`, a token or a condition as the form holds it, to
  `value`, or drop the field for a nil value: a checkbox unticked is
  a field the form does not submit, and one ticked is its `on`, which is
  how the placeholder's true and false arrive."
  [m field value]
  (let [value (cond (true? value) "on" (false? value) nil :else value)]
    (if (some? value) (assoc m field value) (dissoc m field))))

(defn set-condition
  "Set `field` of the condition with `id` of token `i` in `state`,
  counted from one, to `value` as the reader set it (see `with-field`)."
  [state [i id field] value]
  (update-in state [:tokens (dec i) :conditions]
             (fn [conditions]
               (mapv #(if (= id (:id %)) (with-field % field value) %)
                     conditions))))

(defn set-token
  "Set `field` of token `i` in `state`, counted from one, to `value` as
  the reader set it (see `with-field`)."
  [state [i field] value]
  (update-in state [:tokens (dec i)] with-field field value))

(defn switch-mode
  "Change the form of the query in `state` to `mode` from the form's
  fields `live`, holding in the new form as much of the query the old
  one holds as it can, as the server does for a submitted form (see
  dk.cst.corpus-probe.query/arrived), and saying the rest in the form's
  status line. Switching away and back loses nothing while nothing was
  edited: the form the last switch left is `:remembered`."
  [{:keys [params tokens remembered projected] :as state} mode live]
  (let [from    (mode/form params)
        ;; as the form stands, not as it was served: a word typed since
        ;; the last search comes along
        typed   (query/of (assoc live :mode from))
        ;; a form still holding what the last switch handed it gets the
        ;; form that switch left back as it was, words as words
        back?   (and (= typed projected) (= mode (:form remembered)))
        target  (if (= "extended" mode) mode "cqp")
        held    (when-not back? (query/project target typed))
        spelt   (if back?
                  (:params remembered)
                  (query/->params target held))
        ;; what neither form reads stays in the params as memory
        memory  (-> (apply dissoc params (mode/read-keys from params))
                    (merge (select-keys live
                                        (filter mode/query-key? (keys live)))))]
    (assoc state
           :params     (-> (apply dissoc memory (mode/read-keys mode memory))
                           (merge spelt)
                           (assoc :mode mode))
           :tokens     (cond
                         (not= "extended" mode)
                         (tokens/own-rows (query/form-rows nil))
                         back? (:tokens remembered)
                         :else
                         (tokens/own-rows (query/form-rows held)))
           :switch     {:loss   (if back? [] (query/loss target typed))
                        :unread #{}}
           :remembered {:form   from
                        :params (select-keys live (mode/read-keys from live))
                        :tokens tokens}
           :projected  (query/of (assoc spelt :mode mode)))))

(defn submit-on-enter
  "Submit the search from the query field, `state` as it is, when
  `pressed` is Enter and neither `shift?` nor `composing?`: the field is
  a text area, which takes Enter as a line, where a reader pressing it
  after a word expects a submit; so a line is Shift+Enter, as the chat
  boxes have it, and Enter commits the composition while an input method
  is composing."
  [state pressed shift? composing?]
  (if (and (= "Enter" pressed) (not shift?) (not composing?))
    {:state state :effects [[:prevent-default] [:resubmit url/form-id]]}
    {:state state}))

(defn swallow-enter
  "Keep `pressed` from submitting the search, `state` as it is, when it
  is Enter: a text field in a form submits it on Enter, and a reader
  finding something to tick is not asking for an answer yet."
  [state pressed]
  (cond-> {:state state}
    (= "Enter" pressed) (assoc :effects [[:prevent-default]])))

(defn toggle-corpora
  "Select every corpus in `ids` in `state`, or clear them all when they
  are already selected, the change noted for the chooser (see
  dk.cst.corpus-probe.client.lists/tick) and the metadata filters asked
  to refresh: one rule for a corpus and a folder, so a folder only partly
  selected fills rather than clearing the part the reader already had."
  [state ids]
  (let [corpus     (get-in state [:params :corpus])
        unticking? (every? (set corpus) ids)]
    {:state   (-> state
                  (assoc-in [:params :corpus]
                            (lists/select-corpora corpus ids (not unticking?)))
                  (lists/tick :corpora ids unticking?))
     :effects [[:refresh-filters]]}))

(defn clear-fields
  "Take the pattern and the range of `attr` out of `state`, which empties
  its fields, they being what the state holds."
  [state attr]
  (-> state
      (update-in [:filter-controls :patterns] dissoc attr)
      (update-in [:filter-controls :ranges] dissoc attr)))

(defn in-force?
  "True when a pattern or a range narrows `attr` in `state` (see
  dk.cst.corpus-probe.views.search.filter/in-force?)."
  [state attr]
  (filter-views/in-force? (get-in state [:filter-controls :patterns attr])
                          (get-in state [:filter-controls :ranges attr])))

(defn toggle-filter-values
  "Choose or drop the metadata `values` of `attr` in `state` (see
  dk.cst.corpus-probe.client.lists/choose-values), the change noted for
  the filter's list.

  An attribute a pattern or a range narrows is emptied instead, fields
  and boxes together: the control is the one way back from a narrowing
  the boxes cannot show."
  [state attr values]
  (let [chosen (set (get-in state [:filter-controls :selected attr]))
        clear? (or (in-force? state attr) (every? chosen values))]
    (-> state
        (cond-> clear? (clear-fields attr))
        (update-in [:filter-controls :selected]
                   (if clear? lists/drop-values lists/choose-values)
                   attr values)
        (lists/tick :values (map (partial vector attr) values) clear?))))

(defn set-filter-pattern
  "Put `value`, the pattern the values of `attr` must match, in `state`
  as the reader writes it."
  [state attr value]
  (assoc-in state [:filter-controls :patterns attr] value))

(defn set-filter-bound
  "Put `value`, one `end` (:from or :to) of the range the values of
  `attr` must lie in, in `state` as the reader writes it."
  [state attr end value]
  (update-in state [:filter-controls :ranges attr]
             ;; a pair, which the views read as [from to], not a map
             #(assoc (or % [nil nil]) (case end :from 0 :to 1) value)))

(defn clear-filter
  "Empty the whole metadata filter of `state`, the patterns and ranges
  with the values, every value it held noted as unticked for the filter's
  list."
  [state]
  (-> state
      (update :filter-controls assoc :selected {} :patterns {} :ranges {})
      (lists/tick :values (filter-views/filter-pairs
                           (get-in state [:filter-controls :selected]))
                  true)))

(defn refreshed
  "Answer the step with `state`, and ask the metadata filters to refresh
  when the list `k` worked is the metadata filter: what metadata
  a selection offers is fetched rather than known, and only once a
  reader looks at it."
  [k state]
  (cond-> {:state state}
    (= :values k) (assoc :effects [[:refresh-filters]])))

(defn filters-due
  "Fetch the metadata filters the selection of `state` now offers, when
  they are stale (see dk.cst.corpus-probe.client.lists/filters-stale?);
  nothing otherwise."
  [state]
  (if (lists/filters-stale? state)
    {:state   (assoc state :filters-pending? true)
     :effects [[:fetch-filters (lists/chosen-corpora state)]]}
    {:state state}))

(defn filters-arrived
  "Apply `options`, the metadata filters fetched for `corpora`, to
  `state` while they still describe the selection, so that a slow answer
  to a question the reader has moved on from does not overwrite the
  answer to the one they are asking now; `:selected` is the reader's and
  is kept, a chosen value the new corpora do not offer keeping its box."
  [state corpora options]
  (cond-> (assoc state :filters-pending? false)
    (= corpora (lists/chosen-corpora state))
    (-> (assoc :filters-for corpora)
        (update :filter-controls merge (select-keys options [:attrs :unlisted]))
        ;; new attributes, so what the resting view opens is decided
        ;; afresh, unless the reader is in the list by now
        (cond-> (not (get-in state [:lists :values :choosing?]))
          (lists/settle :values)))))

(defn counts-arrived
  "Put `counted`, the count of the search on screen, in `state`, with the
  document title it decides."
  [state counted]
  {:state   (-> state
                (update :result #(-> (merge % (select-keys counted
                                                           [:counts :size
                                                            :pages]))
                                     (dissoc :remaining)))
                (merge (select-keys counted [:prev-href :next-href])))
   :effects [[:set-title (:title counted)]]})

(defn data->state
  "Server `data` as the state this client renders from at the absolute
  `href` it arrived at: marked as the client's, with each list at rest,
  since what a served page shows of the two lists is what the search
  read."
  [data href]
  (let [url (js/URL. href)]
    (-> data
        (assoc :client?     true
               ;; the place in the page the location names, which a
               ;; document marks (see dk.cst.corpus-probe.hiccup/mark-target)
               :fragment    (router/fragment url)
               :lists       (into {}
                                  (for [[k {:keys [tree chosen]}] lists/lists
                                        :let [selected (chosen data)]]
                                    [k {:open      (chooser/open-at-rest
                                                    (tree data selected)
                                                    selected)
                                        :choosing? false
                                        :unticked  #{}}]))
               ;; what the filters on screen describe, so that a selection
               ;; that has changed can be told from one that has not
               :filters-for (lists/chosen-corpora data))
        (update :tokens tokens/own-rows))))

(defn set-preference
  "Store setting `k` as `v` and put the reader where that leaves them,
  who came from `return`.

  Storing the search settings asks the server for nothing, so the page
  stays as it is. Every other preference is fetched again: the language
  because the server words the title and the summaries, a reset because
  the form it leaves behind is the bare one (see
  dk.cst.corpus-probe.settings/return). Onto the history only where it
  took the reader somewhere else."
  [state k v return]
  (let [k (keyword k)]
    (if (and (= settings/cookie-key k) (not (settings/reset? {k v})))
      {:state   (assoc state :stored v :announcement :saved)
       :effects [[:set-cookie k v] [:focus settings/box-id]]}
      (let [to (settings/return {k v :return return})]
        {:state   state
         :effects [[:set-cookie k v] [:navigate to (not= to return)]]}))))

(defn form-changed
  "The state with the settings the form now shows in `params` taken into
  it, stored where the reader has automatic storing on.

  This client answers only the controls it has handlers for and reads
  the rest when a search is sent, so the matching options would
  otherwise never reach what the preferences box measures, and a change
  nobody searched on would be lost on going anywhere else. A corpus
  comes back as one name or several, so it is read as a URL names it."
  [state params]
  (let [shown (cond-> (select-keys params settings/param-keys)
                (contains? params :corpus)
                (update :corpus url/corpora-param))
        state (update state :params merge shown)
        now   (search-views/settings-now state)]
    (if (and (:autosave? state) (not= now (:stored state)))
      {:state   (assoc state :stored now)
       :effects [[:set-cookie settings/cookie-key now]]}
      {:state state})))

(defn set-autosave
  "Turn storing the settings as the reader changes them `on?` or off, and
  store that choice at once (see
  dk.cst.corpus-probe.views.search/autosave-control).

  Turning it on stores the form in front of them, which is what asking
  for it from now on means. Turning it off stores the choice alone: a
  reader turning storing off is not asking for one last store."
  [{:keys [stored selectable] :as state} on?]
  (let [state (assoc state :autosave? on?)
        now   (if on?
                (search-views/settings-now state)
                (settings/string
                 (assoc (url/form-decode stored)
                        settings/autosave-key settings/autosave-off)
                 selectable))]
    {:state   (assoc state :stored now)
     :effects [[:set-cookie settings/cookie-key now]]}))

(defn page-arrived
  "The state of the page `data` fetched from `href` and the effects of
  arriving on it, its address pushed onto the history when `push?`, the
  search `state` was on kept where the page is not one itself."
  [state data href push?]
  ;; the link back to the search is built from the page being rendered,
  ;; so a corpus or a document has none of its own and would drop the
  ;; result at the first step away from it
  (let [cited  (router/cited-href href)
        search (when (not= :search (:route data))
                 (get-in state [:nav :search]))
        data   (cond-> data search (assoc-in [:nav :search] search))]
    {:state   (data->state data cited)
     :effects (cond->> [[:set-title (:title data)]
                        [:set-lang (:lang data)]
                        [:sync-url]
                        [:fetch-counts]
                        [:land]]
                push? (into [[:push-url cited]]))}))

(defn act
  "The state to keep and the effects to run, `{:state state' :effects
  [...]}`, for `action` on `state`: `[kind & args]`, a view's action with
  its placeholders filled in or one an effect dispatched; nil for a kind
  nothing here answers. The render hooks never come this way (see
  dk.cst.corpus-probe.client/dispatch!)."
  [state [kind x y z]]
  ;; an announcement belongs to the act, not to the state it left: taking
  ;; it away here empties the live region on the reader's next move, so
  ;; the same thing said twice is heard twice
  (let [state (dissoc state :announcement)]
    (case kind
      :set-mode             {:state (switch-mode state x y)}
      :add-token            (add-token state)
      :remove-token         (remove-token state x)
      :add-condition        (add-condition state x)
      :remove-condition     (let [[i id] x] (remove-condition state i id))
      ;; so the answer can tell when the form has moved on from what ran;
      ;; the field keeps what was typed, since Replicant leaves an
      ;; unchanged value alone
      :set-query            {:state (assoc-in state [:params :q] x)}
      :submit-on-enter      (submit-on-enter state x y z)
      :set-condition        {:state (set-condition state x y)}
      :set-token            {:state (set-token state x y)}
      :apply-view           {:state state :effects [[:resubmit url/form-id]]}
      :toggle-corpora       (toggle-corpora state x)
      :toggle-filter-values (let [[attr values] x]
                              {:state (toggle-filter-values state attr values)})
      :set-filter-pattern   {:state (set-filter-pattern state x y)}
      :set-filter-bound     {:state (set-filter-bound state x y z)}
      :clear-filter         {:state (clear-filter state)}
      :engage               (refreshed x (lists/engage state x))
      :toggle-open          (refreshed x (lists/toggle-open state x y z))
      :filter               {:state (lists/apply-filter state x y)}
      :leave                {:state (cond-> state y (lists/leave x))}
      :swallow-enter        (swallow-enter state x)
      ;; on focus as well as on click, so the panel follows the cursor
      :inspect              {:state (inspect state x y)}
      :close                (close state)
      :move-cursor          (move-cursor state x y z)
      :leave-concordance    {:state state :effects [[:leave-concordance]]}
      :recentre             {:state state :effects [[:recentre]]}
      :filters-due          (filters-due state)
      :filters-arrived      {:state (filters-arrived state x y)}
      :filters-failed       {:state (assoc state :filters-pending? false)}
      :counts-arrived       (counts-arrived state x)
      :wider-arrived        (wider-arrived state x y)
      ;; the fetch is gone, so a later step at the end may ask again
      :wider-failed         {:state (update state :result dissoc :widening)}
      :page-arrived         (page-arrived state x y z)
      :pending              {:state (assoc state :pending? true)}
      :set-fragment         {:state (assoc state :fragment x)}
      :navigate             {:state state :effects [[:navigate x y]]}
      :form-changed         (form-changed state x)
      :set-autosave         (set-autosave state x)
      :set-preference       (set-preference state x y z)
      nil)))
