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
            [dk.cst.corpus-probe.views.kwic :as kwic]
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
        (.catch (fn [_] (collapse! k))))))

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
    (let [{:keys [corpus cpos matchend]} arg
          k (kwic/hit-key arg)]
      (if (contains? (:expanded @state) k)
        (swap! state update :expanded dissoc k)
        ;; commit intent immediately (loading placeholder) so the toggle and
        ;; the URL reflect the click at once and a duplicate fetch is suppressed
        (do (swap! state assoc-in [:expanded k] ::loading)
            (fetch-context! corpus cpos matchend))))
    nil))

(defn sync-popover!
  "Open or close the token popover to match `selected?`; the guards avoid the
  errors the Popover API throws on a redundant show/hide."
  [selected?]
  (when-let [el (.getElementById js/document "token-details")]
    (cond
      (and selected? (not (.matches el ":popover-open"))) (.showPopover el)
      (and (not selected?) (.matches el ":popover-open")) (.hidePopover el))))

(defn sync-expand-url!
  "Mirror the expanded hits in the URL's `expand` parameter as
  `CORPUS:cpos` items, replacing history so the URL stays shareable without
  new entries."
  []
  (let [url (current-url)
        ks  (sort (keys (:expanded @state)))]
    (if (seq ks)
      (.set (.-searchParams url) "expand"
            (str/join "," (map (fn [[corpus cpos]] (str corpus ":" cpos))
                               ks)))
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
  "The set of hit keys named in the URL's `expand` parameter (`CORPUS:cpos`
  items); malformed items are ignored."
  []
  (when-let [param (.get (.-searchParams (current-url)) "expand")]
    (set (keep (fn [item]
                 (let [[corpus cpos] (str/split item #":" 2)]
                   (when-let [n (some-> cpos parse-long)]
                     [corpus n])))
               (str/split param #",")))))

(defn wanted-hits
  "The loaded hits whose key is in `wanted` (nil when nothing is wanted);
  `?expand` is scoped to the current page, so off-page hits are ignored."
  [wanted]
  (when wanted
    (filter (comp wanted kwic/hit-key)
            (get-in @state [:result :hits]))))

(defn init!
  "Boot the client: seed state from the bootstrap payload, wire dispatch,
  re-render on every state change, and restore any expansions from the URL.

  The wanted hits are seeded as loading placeholders before the first
  render, so that render's URL sync keeps the `expand` parameter rather than
  wiping it before the restore fetches run."
  []
  (reset! state (read-payload))
  (let [hits (wanted-hits (expand-param))]
    (when (seq hits)
      (swap! state assoc :expanded
             (into {} (map (fn [hit] [(kwic/hit-key hit) ::loading])) hits)))
    (r/set-dispatch! handle!)
    (add-watch state ::render (fn [_ _ _ _] (render!)))
    (render!)
    (doseq [{:keys [corpus cpos anchors]} hits]
      (fetch-context! corpus cpos (:matchend anchors)))))
