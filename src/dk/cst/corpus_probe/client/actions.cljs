(ns dk.cst.corpus-probe.client.actions
  "The pure step of the client: `act` takes the state and an action and
  answers with the state to keep and the effects to run. Nothing here
  touches the document, a timer, the network or the history."
  (:require [dk.cst.corpus-probe.client.lists :as lists]
            [dk.cst.corpus-probe.client.router :as router]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.query.tokens :as tokens]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.chooser :as chooser]
            [dk.cst.corpus-probe.views.concordance :as concordance]
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

(defn cursor-rows
  "Every row the cursor can visit in `state`, in the order they are read:
  each hit's own row and, beneath it, its wider context while one is
  showing, as {:key hit-key :hit hit :from index-of-first-token}; an
  expanded row numbers its tokens past the row it expands, so one index
  names one token across both."
  [state]
  (let [expanded (:expanded state)]
    (mapcat (fn [hit]
              (let [k  (concordance/hit-key hit)
                    ex (get expanded k)]
                (cond-> [{:key k :hit hit :from 0}]
                  (map? ex) (conj {:key  k
                                   :hit  ex
                                   :from (concordance/token-count hit)}))))
            (hits state))))

(defn row-at
  "The index in `rows` of the row the cursor `[k i]` is in: the last row of
  that hit whose tokens start at or before `i`."
  [rows [k i]]
  (or (last (keep-indexed (fn [n {:keys [key from]}]
                            (when (and (= key k) (<= from i)) n))
                          rows))
      0))

(defn step-cursor
  "The cursor moved `[rows tokens]` through `rows*` (see `cursor-rows`):
  along a row it stops at its ends rather than wrapping; between rows it
  keeps its distance from the match rather than its column, so stepping
  into a hit's wider context lands on the word the cursor was on, or its
  nearest token where the rows differ."
  [rows* cursor [rows tokens]]
  (let [at   (row-at rows* cursor)
        here (nth rows* at)
        i    (- (second cursor) (:from here))]
    (if (zero? rows)
      (let [i* (min (dec (concordance/token-count (:hit here)))
                    (max 0 (+ i tokens)))]
        [(:key here) (+ (:from here) i*)])
      (let [n      (min (dec (count rows*)) (max 0 (+ at rows)))
            there  (nth rows* n)
            offset (concordance/token->offset (:hit here) i)]
        [(:key there)
         (+ (:from there) (concordance/offset->token (:hit there) offset))]))))

(defn cursor-id
  "The id of the token `cursor` is on (see
  dk.cst.corpus-probe.views.concordance/token-id), which focus follows."
  [[[corpus cpos] i]]
  (concordance/token-id {:corpus corpus :cpos cpos} i))

(defn move-cursor
  "Answer key `pressed` on the token at cursor `k` in `state`: an arrow
  moves the cursor and focus with it, Home and End go to the ends of the
  row the cursor is in, and Escape closes the panel; each consumed, since
  the concordance is one tab stop with a cursor inside it."
  [state k pressed]
  (let [rows (cursor-rows state)
        move (fn [cursor]
               {:state   (assoc state :cursor cursor)
                :effects [[:prevent-default] [:focus (cursor-id cursor)]]})]
    (cond
      (= "Escape" pressed)
      {:state (dissoc state :selected) :effects [[:prevent-default]]}

      (contains? arrow-keys pressed)
      (move (step-cursor rows k (arrow-keys pressed)))

      ;; the ends of the row the cursor is in, not of the hit: a wider
      ;; context is its own run of text
      (contains? #{"Home" "End"} pressed)
      (let [{:keys [key hit from]} (nth rows (row-at rows k))
            i (if (= "Home" pressed) 0 (dec (concordance/token-count hit)))]
        (move [key (+ from i)]))

      :else {:state state})))

(defn inspect
  "`state` with `selected`, the token the inspection panel describes, or
  without one for nil."
  [state selected]
  (if selected
    (assoc state :selected selected)
    (dissoc state :selected)))

(defn close
  "Dismiss the inspection panel of `state` from its own button and leave
  focus in the concordance it describes, rather than on a token."
  [state]
  {:state   (dissoc state :selected)
   ;; not a token, whose focus would open the panel again, and not the
   ;; button, which is about to go: the region, focusable since it
   ;; scrolls, from where a tab reaches the cursor again
   :effects [[:focus concordance/region-id]]})

(defn collapse
  "`state` without the hit keyed `k` in the expanded set, if it is there."
  [state k]
  (cond-> state
    (contains? (:expanded state) k) (update :expanded dissoc k)))

(defn toggle-context
  "Expand `hit` in `state`, fetching its wider context, or collapse it
  when it is expanded; the URL follows either way."
  [state {:keys [corpus cpos matchend] :as hit}]
  (let [k (concordance/hit-key hit)]
    (if (contains? (:expanded state) k)
      {:state   (update state :expanded dissoc k)
       :effects [[:sync-url]]}
      ;; the placeholder at once, so a second click does not fetch twice
      {:state   (assoc-in state [:expanded k] concordance/loading)
       :effects [[:fetch-context corpus cpos matchend] [:sync-url]]})))

(defn context-arrived
  "`state` with `hit`, the wider context fetched for the hit keyed `k`,
  in the expanded set if it is still wanted there; a hit collapsed while
  the fetch was in flight, or an empty answer, collapses the entry
  again, so a late response never revives a dismissed hit."
  [state k hit]
  (if (and hit (contains? (:expanded state) k))
    {:state (assoc-in state [:expanded k] hit)}
    {:state   (collapse state k)
     :effects [[:sync-url]]}))

(defn context-failed
  "`state` with the hit keyed `k` marked as one whose context could not
  be fetched (see dk.cst.corpus-probe.views.concordance/failed), if it
  is still expanded."
  [state k]
  (cond-> state
    (contains? (:expanded state) k)
    (assoc-in [:expanded k] concordance/failed)))

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

(defn add-token
  "`state` with a blank token after the last, and focus on its attribute."
  [{:keys [tokens] :as state}]
  (let [id (inc (reduce max 0 (map :id tokens)))]
    {:state   (assoc state :tokens (conj (vec tokens) (tokens/blank-token id)))
     :effects [[:focus-field (tokens/token-key (inc (count tokens))
                                               1 :attr)]]}))

(defn remove-token
  "`state` without the token with `id`, and focus on the attribute of the
  token now in its place, or of the last."
  [{:keys [tokens] :as state} id]
  (let [k    (place tokens id)
        left (without tokens id (tokens/blank-token (inc id)))]
    {:state   (assoc state :tokens left)
     :effects [[:focus-field (tokens/token-key (min k (count left))
                                               1 :attr)]]}))

(defn add-condition
  "`state` with a blank condition added to token `i`, counted from one,
  and focus on its join."
  [state i]
  (let [path       [:tokens (dec i) :conditions]
        conditions (vec (get-in state path))
        id         (inc (reduce max 0 (map :id conditions)))]
    {:state   (assoc-in state path (conj conditions {:id id}))
     :effects [[:focus-field (tokens/token-key i (inc (count conditions))
                                               :join)]]}))

(defn remove-condition
  "`state` without the condition with `id` of token `i`, counted from
  one, and focus on the condition now in its place, or the last: its
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
  "`m`, a token or a condition as the form holds it, with `field` set to
  `value`, or without the field for a nil value: a checkbox unticked is
  a field the form does not submit, and one ticked is its `on`, which is
  how the placeholder's true and false arrive."
  [m field value]
  (let [value (cond (true? value) "on" (false? value) nil :else value)]
    (if (some? value) (assoc m field value) (dissoc m field))))

(defn set-condition
  "`state` with `field` of the condition with `id` of token `i`, counted
  from one, set to `value` as the reader set it (see `with-field`)."
  [state [i id field] value]
  (update-in state [:tokens (dec i) :conditions]
             (fn [conditions]
               (mapv #(if (= id (:id %)) (with-field % field value) %)
                     conditions))))

(defn set-token
  "`state` with `field` of token `i`, counted from one, set to `value` as
  the reader set it (see `with-field`)."
  [state [i field] value]
  (update-in state [:tokens (dec i)] with-field field value))

(defn switch-mode
  "`state` with the form of its query changed to `mode` from the form's
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
  "`state` as it is, and the search submitted from the query field when
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
  "`state` as it is, with `pressed` kept from submitting the search when
  it is Enter: a text field in a form submits it on Enter, and a reader
  finding something to tick is not asking for an answer yet."
  [state pressed]
  (cond-> {:state state}
    (= "Enter" pressed) (assoc :effects [[:prevent-default]])))

(defn toggle-corpora
  "`state` with every corpus in `ids` selected, or all of them cleared
  when they are already selected, the change noted for the chooser (see
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

(defn toggle-filter-values
  "`state` with the metadata `values` of `attr` chosen or dropped (see
  dk.cst.corpus-probe.client.lists/choose-values), the change noted for
  the filter's list."
  [state attr values]
  (let [chosen (set (get-in state [:filter-controls :selected attr]))]
    (-> state
        (update-in [:filter-controls :selected]
                   lists/choose-values attr values)
        (lists/tick :values (map (partial vector attr) values)
                    (every? chosen values)))))

(defn clear-filter
  "`state` with the whole metadata filter emptied, every value it held
  noted as unticked for the filter's list."
  [state]
  (-> state
      (assoc-in [:filter-controls :selected] {})
      (lists/tick :values (filter-views/filter-pairs
                           (get-in state [:filter-controls :selected]))
                  true)))

(defn refreshed
  "`state` as the step's answer, with the metadata filters asked to
  refresh when the list `k` worked is the metadata filter: what metadata
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
  "`state` with `options`, the metadata filters fetched for `corpora`,
  applied while they still describe the selection, so that a slow answer
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
  "`state` with `counted`, the count of the search on screen, and the
  document title it decides."
  [state counted]
  {:state   (-> state
                (update :result #(-> (merge % (select-keys counted
                                                           [:counts :size
                                                            :pages]))
                                     (dissoc :remaining)))
                (merge (select-keys counted [:prev-href :next-href])))
   :effects [[:set-title (:title counted)]]})

(defn wanted-hits
  "The hits of `data` whose key is in `wanted` (nil when nothing is
  wanted); `?expand` is scoped to one page, so off-page hits are ignored."
  [data wanted]
  (when wanted
    (filter (comp wanted concordance/hit-key) (get-in data [:result :hits]))))

(defn with-expansions
  "`data` with the hits keyed in `wanted` seeded as loading placeholders,
  which the fetch of the expansions reads."
  [data wanted]
  (let [hits (wanted-hits data wanted)]
    (cond-> data
      (seq hits) (assoc :expanded
                        (into {}
                              (map (fn [hit]
                                     [(concordance/hit-key hit)
                                      concordance/loading]))
                              hits)))))

(defn data->state
  "Server `data` as the state this client renders from at the absolute
  `href` it arrived at: marked as the client's, seeded with the
  expansions the URL names, and with each list at rest, since what a
  served page shows of the two lists is what the search read."
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
        (update :tokens tokens/own-rows)
        (with-expansions (url/expand-param
                          (:expand (router/url-params url)))))))

(defn page-arrived
  "The state of the page `data` fetched from `href` and the effects of
  arriving on it, its address pushed onto the history when `push?`."
  [data href push?]
  (let [cited (router/cited-href href)]
    {:state   (data->state data cited)
     :effects (cond->> [[:set-title (:title data)]
                        [:set-lang (:lang data)]
                        [:sync-url]
                        [:fetch-expansions]
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
    :clear-filter         {:state (clear-filter state)}
    :engage               (refreshed x (lists/engage state x))
    :toggle-open          (refreshed x (lists/toggle-open state x y z))
    :filter               {:state (lists/apply-filter state x y)}
    :leave                {:state (cond-> state y (lists/leave x))}
    :swallow-enter        (swallow-enter state x)
    ;; on focus as well as on click, so the panel follows the cursor
    :inspect              {:state (inspect state x)}
    :close                (close state)
    :move-cursor          (move-cursor state x y)
    :leave-concordance    {:state state :effects [[:leave-concordance]]}
    :toggle-context       (toggle-context state x)
    :context-arrived      (context-arrived state x y)
    :context-failed       {:state (context-failed state x)}
    :filters-due          (filters-due state)
    :filters-arrived      {:state (filters-arrived state x y)}
    :filters-failed       {:state (assoc state :filters-pending? false)}
    :counts-arrived       (counts-arrived state x)
    :page-arrived         (page-arrived x y z)
    :pending              {:state (assoc state :pending? true)}
    :set-fragment         {:state (assoc state :fragment x)}
    :navigate             {:state state :effects [[:navigate x y]]}
    :set-preference       {:state state :effects [[:set-preference x y]]}
    nil))
