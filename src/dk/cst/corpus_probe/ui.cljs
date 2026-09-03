(ns dk.cst.corpus-probe.ui
  "Client entry point: read the bootstrap payload the server embedded, mount
  Replicant on the server-rendered page and re-render it from application
  state so token clicks reveal the inspection popover and a hit's corpus
  position expands its context.

  Interactivity is progressive: without this script the page is a working
  server-rendered concordance; with it, the same views become live. The set
  of expanded hits is mirrored in the URL's `expand` parameter, so an
  expanded view survives a reload and can be shared."
  (:require [clojure.string :as str]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.views.page :as page]
            [replicant.dom :as r]))

(defonce state
  (atom nil))

(defn read-payload
  "Read the transit payload from the #bootstrap script element."
  []
  (let [s (.-textContent (.getElementById js/document "bootstrap"))]
    (transit/read (transit/reader :json) s)))

(defn current-url
  "The current location as a mutable URL object."
  []
  (js/URL. js/location.href))

(defn collapse!
  "Remove `cpos` from the expanded set if it is still there."
  [cpos]
  (when (contains? (:expanded @state) cpos)
    (swap! state update :expanded dissoc cpos)))

(defn fetch-context!
  "Fetch the hit at `cpos`/`matchend` in `corpus` with wider context and, if
  it is still wanted, store it under `:expanded` `cpos`.

  A failed request or a hit the user collapsed while the fetch was in flight
  collapses the entry again, so a late response never revives a hit the user
  dismissed."
  [corpus cpos matchend]
  (-> (js/fetch (str "/api/context?corpus=" corpus
                     "&cpos=" cpos "&matchend=" matchend))
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (throw (js/Error. "context request failed")))))
      (.then (fn [body]
               (let [hit (transit/read (transit/reader :json) body)]
                 (if (and hit (contains? (:expanded @state) cpos))
                   (swap! state assoc-in [:expanded cpos] hit)
                   (collapse! cpos)))))
      (.catch (fn [_] (collapse! cpos)))))

(defn handle!
  "Apply an `action` to the state, using `data` (the Replicant dispatch data)
  for the popover's toggle event.

  `:popover-toggle` fires for every open and close; only a close the browser
  initiated (light-dismiss or Escape) while a token is still selected needs
  to clear the selection. `:toggle-context` expands a hit (fetching its wider
  context) or collapses it."
  [data [action arg]]
  (case action
    :inspect (swap! state assoc :selected arg)
    :close   (swap! state dissoc :selected)
    :popover-toggle
    (when (and (= "closed" (.-newState (:replicant/dom-event data)))
               (:selected @state))
      (swap! state dissoc :selected))
    :toggle-context
    (let [{:keys [cpos matchend]} arg]
      (if (contains? (:expanded @state) cpos)
        (swap! state update :expanded dissoc cpos)
        ;; commit intent immediately (loading placeholder) so the toggle and
        ;; the URL reflect the click at once and a duplicate fetch is suppressed
        (do (swap! state assoc-in [:expanded cpos] ::loading)
            (fetch-context! (get-in @state [:params :corpus]) cpos matchend))))
    nil))

(defn sync-popover!
  "Open or close the token popover to match `selected?`; the guards avoid the
  errors the Popover API throws on a redundant show/hide."
  [selected?]
  (when-let [el (.getElementById js/document "token-details")]
    (cond
      (and selected? (not (.matches el ":popover-open")))     (.showPopover el)
      (and (not selected?) (.matches el ":popover-open"))     (.hidePopover el))))

(defn sync-expand-url!
  "Mirror the expanded corpus positions in the URL's `expand` parameter,
  replacing history so the URL stays shareable without new entries."
  []
  (let [url    (current-url)
        cposes (sort (keys (:expanded @state)))]
    (if (seq cposes)
      (.set (.-searchParams url) "expand" (str/join "," cposes))
      (.delete (.-searchParams url) "expand"))
    (.replaceState js/history nil "" (.-href url))))

(defn render!
  "Render the current state into #app, then sync the popover and URL to it."
  []
  ;; TODO: the first render clears #app and rebuilds the server-rendered
  ;; markup rather than adopting it, so in-flight form edits during script
  ;; load are lost. Replicant has SSR hydration coming; adopt the existing
  ;; DOM here once it lands.
  (r/render (.getElementById js/document "app") (page/app-view @state))
  (sync-popover! (boolean (:selected @state)))
  (sync-expand-url!))

(defn expand-param
  "The set of corpus positions named in the URL's `expand` parameter."
  []
  (when-let [param (.get (.-searchParams (current-url)) "expand")]
    (set (keep parse-long (str/split param #",")))))

(defn wanted-hits
  "The loaded hits whose position is in `wanted` (nil when nothing is
  wanted); `?expand` is scoped to the current page, so off-page positions
  are ignored."
  [wanted]
  (when wanted
    (filter (fn [{:keys [cpos]}] (wanted cpos))
            (get-in @state [:result :hits]))))

(defn init!
  "Boot the client: seed state from the bootstrap payload, wire dispatch,
  re-render on every state change, and restore any expansions from the URL.

  The wanted positions are seeded as loading placeholders before the first
  render, so that render's URL sync keeps the `expand` parameter rather than
  wiping it before the restore fetches run."
  []
  (reset! state (read-payload))
  (let [hits   (wanted-hits (expand-param))
        corpus (get-in @state [:params :corpus])]
    (when (seq hits)
      (swap! state assoc :expanded
             (into {} (map (fn [{:keys [cpos]}] [cpos ::loading])) hits)))
    (r/set-dispatch! handle!)
    (add-watch state ::render (fn [_ _ _ _] (render!)))
    (render!)
    (doseq [{:keys [cpos anchors]} hits]
      (fetch-context! corpus cpos (:matchend anchors)))))
