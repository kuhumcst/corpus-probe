(ns dk.cst.corpus-probe.client
  "Client entry point: read the bootstrap payload the server embedded,
  mount Replicant on the server-rendered page, and take over navigation
  so moving between views swaps the page's data rather than reloading
  it. Every action a view dispatches is data, answered by the pure step
  (dk.cst.corpus-probe.client.actions) whose effects run at the edge
  (dk.cst.corpus-probe.client.effects); `dispatch!` joins the two and
  keeps the reader's focus (dk.cst.corpus-probe.client.focus)."
  (:require [clojure.walk :as walk]
            [dk.cst.corpus-probe.client.actions :as actions]
            [dk.cst.corpus-probe.client.effects :as effects]
            [dk.cst.corpus-probe.client.focus :as focus]
            [dk.cst.corpus-probe.client.lists :as lists]
            [dk.cst.corpus-probe.client.router :as router]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.views :as views]
            [replicant.dom :as r]))

(defonce ^{:doc "The application state, which every render reads and
  `dispatch!` writes."}
  state
  (atom nil))

(def placeholders
  "What a view puts in an action where it needs something only the event
  knows, each read from the Replicant dispatch data of the event. An
  action stays data in the hiccup, which is what lets a render leave an
  unchanged control alone."
  {:event.target/value       (fn [{:replicant/keys [node]}]
                               (.-value node))
   :event.target/checked     (fn [{:replicant/keys [node]}]
                               (.-checked node))
   :event.target/open        (fn [{:replicant/keys [node]}]
                               (.-open node))
   :event/key                (fn [{:replicant/keys [dom-event]}]
                               (.-key dom-event))
   :event/shift?             (fn [{:replicant/keys [dom-event]}]
                               (.-shiftKey dom-event))
   :event/ctrl?              (fn [{:replicant/keys [dom-event]}]
                               (.-ctrlKey dom-event))
   :event/composing?         (fn [{:replicant/keys [dom-event]}]
                               (.-isComposing dom-event))
   :event.target.form/params (fn [{:replicant/keys [node]}]
                               (router/form-params (.-form node)))
   :event/focus-left?        (fn [{:replicant/keys [dom-event]}]
                               (focus/focus-left? dom-event))})

(defn interpolate
  "Replace every placeholder in `action` (see `placeholders`) with what
  the dispatch `data` of its event says."
  [data action]
  (walk/postwalk (fn [x]
                   (if-let [f (and (keyword? x) (placeholders x))]
                     (f data)
                     x))
                 action))

(defn read-payload
  "Read the transit payload from the #bootstrap script element, or nil on a
  page the server shipped none for."
  []
  (some-> (.getElementById js/document "bootstrap")
          (.-textContent)
          (effects/read-transit)))

(defn render!
  "Render the state into the masthead, #app and the footer."
  []
  ;; the first render rebuilds the server-rendered markup rather than
  ;; adopting it (replicant.dom/render replaces what it finds, and
  ;; 2026.07.1 has no adoption API), so what a reader typed or opened
  ;; while the script loaded is lost
  (let [{:keys [lang path nav pending?] :as current} @state
        ui (i18n/->ui lang)]
    ;; the masthead carries the current search and the navigation in
    ;; flight, the footer the UI language, so both re-render with the page
    (r/render (.getElementById js/document "masthead")
              (views/site-header ui path nav pending?))
    (r/render (.getElementById js/document "app") (views/page current))
    (r/render (.getElementById js/document "footer") (views/site-footer ui))))

(defn dispatch!
  "Answer `action` for the Replicant dispatch `data` of the event or
  life-cycle hook that raised it, or for none, as the listeners and the
  fetches raise theirs. A control the action takes from under the reader
  hands focus to its box (see dk.cst.corpus-probe.client.focus/rescue!),
  and a step that moves part of the page whole renders inside a view
  transition (see dk.cst.corpus-probe.client.actions/moves)."
  ([action]
   (dispatch! {} action))
  ([{:replicant/keys [trigger] :as data} action]
   ;; a hook's ask is written to its node without touching the state, so
   ;; nothing renders from inside a render
   (if (= :replicant.trigger/life-cycle trigger)
     (effects/perform! dispatch! data [action])
     (let [before @state
           ;; where the reader is, read before the render moves anything
           refuge (focus/refuge)]
       (when-let [{after :state :keys [effects]}
                  (actions/act before (interpolate data action))]
         (let [move  (actions/move before after)
               stop? (comp #{:prevent-default} first)
               ;; the views fade nothing while the browser pictures the
               ;; render: mid-fade is the wrong picture
               after (cond-> after move (assoc :move move))
               step! (fn []
                       (when-not (identical? before after)
                         (reset! state after))
                       ;; once the watch has rendered, so that a focus
                       ;; finds the node the render has made
                       (effects/perform! dispatch! (assoc data :state after)
                                         (remove stop? effects))
                       ;; last, so that an effect that put the reader
                       ;; somewhere stands
                       (focus/rescue! refuge))]
           ;; first and in this call: the browser answers the event until
           ;; the handler returns, while a step that moves waits for the
           ;; frame of its picture. A change landing in that frame is
           ;; lost when the step lands, which is what a picture costs
           (effects/perform! dispatch! data (filter stop? effects))
           (if move
             (effects/view-transition! move step!)
             (step!))))))))

(defn ^:dev/after-load reload!
  "Render again once shadow-cljs has swapped in recompiled code: the
  views are called from the state watcher, and a recompile changes the
  code without changing the state."
  []
  (render!))

(defn init!
  "Boot the client: seed the state from the bootstrap payload and the
  searches the browser remembers, install the listeners and the
  dispatch, render on every change of the state, and arrive on the page
  as a fetched one does."
  []
  ;; the views render tokens and disclosures as controls only where this
  ;; script is running to answer them
  (let [;; a result the reader arrived at directly is a search of theirs
        ;; like any other, and is remembered before the first render
        booted (actions/remember
                (assoc (actions/data->state (read-payload) js/location.href)
                       :recent (effects/read-recent!)))]
    (reset! state (:state booted))
    (router/listen! dispatch! (keys lists/lists))
    (r/set-dispatch! dispatch!)
    (add-watch state ::render (fn [_ _ _ _] (render!)))
    (render!)
    ;; motion starts after this render, which rebuilds the page and must
    ;; not fade it in
    (effects/settle! dispatch!)
    (effects/perform! dispatch! {:state @state}
                      (into [[:sync-url]] (:effects booted)))))
