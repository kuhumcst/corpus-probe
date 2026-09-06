;; The server of the development environment, loaded before the nREPL by
;; the :serve alias (see deps.edn), which names dev/watch.edn as the
;; configuration so that a shadow-cljs watch is let through the
;; Content-Security-Policy.
(require '[dk.cst.corpus-probe.server :as server])
(server/-main)
