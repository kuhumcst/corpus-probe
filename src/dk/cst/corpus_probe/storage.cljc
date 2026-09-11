(ns dk.cst.corpus-probe.storage
  "What the app keeps in a reader's browser between visits, and how.

  Two places, and what reads it decides which. A cookie holds what the
  server must know as it answers: the reader's language, and the
  settings a bare form is seeded from (see
  dk.cst.corpus-probe.storage.settings). The browser's own store holds
  what only the reader's page reads (see
  dk.cst.corpus-probe.storage.recent). A cookie travels with every
  request the browser makes and holds four kilobytes at most, so nothing
  long, and nothing that is a list, belongs in one."
  (:require [clojure.string :as str]))

(def cookie-max-age
  "How long a stored preference outlives the visit that set it, in
  seconds: a year, so a reader states it once."
  31536000)

(defn cookie
  "The Set-Cookie string storing `v` under setting `k` for
  `cookie-max-age`, site-wide and on same-site requests only, as the
  server writes it in a header and the client to the document."
  [k v]
  ;; a setting stored as nothing is forgotten, so storing and clearing
  ;; are one path and a reset needs no writer of its own
  (str (name k) "=" v ";Path=/"
       ";Max-Age=" (if (str/blank? (str v)) 0 cookie-max-age)
       ";SameSite=Lax"))

(comment
  (cookie :settings "corpus=PROBE")
  ;; => "settings=corpus=PROBE;Path=/;Max-Age=31536000;SameSite=Lax"

  (cookie :settings "")
  ;; => "settings=;Path=/;Max-Age=0;SameSite=Lax"

  #_.)
