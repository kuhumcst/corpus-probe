(ns dk.cst.corpus-probe.client.focus
  "Where the reader is and where the client puts them: every rule of
  focus. A landing is a place the client may put the reader that is no
  tab stop (see dk.cst.corpus-probe.views.widgets/landing-attrs). One
  rule asks nothing of an action, `rescue!`: a control that goes away or
  goes quiet under the reader hands focus to the nearest landing that
  survives. An action that knows a better place says so with an effect
  (see dk.cst.corpus-probe.client.effects/perform!), and focus leaving a
  pool of elements, or a list, is read as the reader going elsewhere."
  (:require [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.result :as result-views]
            [dk.cst.corpus-probe.views.search :as search]
            [dk.cst.corpus-probe.views.widgets :as widgets]))

(defn refuge
  "Where the reader is before an action: the active element and the ids
  of the landings it stands in, the nearest first, read while they all
  still stand, so that `rescue!` can find the nearest that survived the
  render (see dk.cst.corpus-probe.views.widgets/landing-attrs)."
  []
  (let [landing #(some-> % (.closest "[id][tabindex=\"-1\"]"))
        el      (.-activeElement js/document)]
    {:active   el
     :landings (->> (landing el)
                    (iterate #(landing (.-parentElement %)))
                    (take-while some?)
                    (mapv #(.-id %)))}))

(defn lost?
  "True when `el` can hold focus no longer: it left the document, or it
  was disabled."
  [el]
  ;; an element leaving behind a transition (:replicant/unmounting) is
  ;; still connected here, so a control in it is handed on by an effect
  ;; instead, as the card's close button is
  (or (not (.-isConnected el)) (.matches el ":disabled")))

(defn rescue!
  "Put the reader on the nearest landing of `refuge` (see `refuge`) that
  survived the action, where the control they were on did not: a browser
  drops focus from a control it disables or takes out, and a reader
  dropped on the document has lost their place."
  [{:keys [active landings]}]
  (let [now (.-activeElement js/document)]
    (when (and (lost? active)
               ;; unless an effect, or the reader, has put focus somewhere
               ;; already
               (or (identical? now active)
                   (identical? now (.-body js/document))))
      (some-> (some #(.getElementById js/document %) landings)
              ;; the landing is around where they were, so nothing scrolls
              (.focus #js {:preventScroll true})))))

(defn focus-left?
  "True when focusout `event` says focus left the element listening: it
  went to a tab stop outside it."
  [event]
  (let [to (.-relatedTarget event)]
    (boolean (and to
                  ;; a press on a label sends focus to a landing, <main>,
                  ;; for a moment before the box it is for takes it
                  (<= 0 (.-tabIndex to))
                  (not (.contains (.-currentTarget event) to))))))

(defn in-form?
  "True when `el` is a control of the search form."
  [el]
  (= url/form-id (some-> el (.-form) (.-id))))

(defn focus!
  "Move focus to the element with `id`, once the render that put it there
  has run, `still?` keeping the page where it is rather than letting the
  browser bring the element into view; the element, for a caller that
  brings it into view itself."
  ([id]
   (focus! id false))
  ([id still?]
   (when-let [el (.getElementById js/document id)]
     (.focus el #js {:preventScroll still?})
     el)))

(defn focus-field!
  "Move focus to the form control named `name`, once the render that put
  it there has run: where a reader who added or took away a token or a
  condition is left, rather than on the body."
  [name]
  (some-> (.querySelector js/document (str "[name=\"" name "\"]"))
          (.focus)))

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
  if they are not already at hand; else at the start of the main content."
  []
  ;; focus, not only a scroll: a routed navigation gives none of the
  ;; announcement and reset of focus a real one does
  ;; nothing has gone where a control of the search form still holds
  ;; focus: the form outlives a search, and taking the caret out of the
  ;; field the reader typed in is no rescue
  (let [hash   (.-hash js/location)
        target (.getElementById js/document url/results-id)
        held?  (in-form? (.-activeElement js/document))]
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
            (focus! url/results-id true)))

      :else
      (do (.scrollTo js/window 0 0)
          (when-not held?
            (focus! widgets/main-id true))))))

(defn select-query!
  "Select what the query field holds, where the search in `state` found
  nothing and the reader asked for it themselves from the field or its
  button: what they try instead replaces what did not work, in one
  keystroke."
  [state]
  ;; not from a control beside the result, where the reader is working,
  ;; nor for a page arrived at by a link, which is no search of theirs
  (let [active (.-activeElement js/document)
        asked? (and (in-form? active)
                    (or (= search/query-id (.-id active))
                        (= "submit" (.-type active))))
        result (:result state)]
    (when (and asked? (not (result-views/counting? result))
               (not (result-views/found? result)))
      (some-> (focus! search/query-id) (.select)))))

(defn once-left!
  "Dispatch `action` through `dispatch!` once focus has settled outside
  every element with `ids`, which are one pool: focus moving between them
  is not leaving."
  [dispatch! ids action]
  ;; a tick later, since focusout fires before the next element has
  ;; focus; and activeElement rather than relatedTarget, so that a click
  ;; on the page background counts as leaving while switching windows,
  ;; which keeps the active element, does not
  (js/setTimeout
   (fn []
     (let [el (.-activeElement js/document)]
       (when-not (some #(some-> (.getElementById js/document %) (.contains el))
                       ids)
         (dispatch! action))))
   0))
