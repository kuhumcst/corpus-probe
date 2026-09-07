(ns dk.cst.corpus-probe.server.response
  "The responses the handlers answer with: a page as the HTML document
  its route renders, or as the same view data in transit when the client
  router asked for it, the downloads, the classpath files and the plain
  refusals.

  Responses are rendered from the shared .cljc views with Replicant's
  string renderer, so the client renders identical markup. The view data
  is embedded in the document as transit for the client to take over
  from. Hostile corpus content survives the round trip because each
  channel is protected: `correct-quote-escaping` fixes the SSR body,
  transit-JSON escapes true control bytes (a carriage return) in the
  payload, and `script-safe` escapes `<` (which transit passes through
  verbatim) so a token containing `</script>` cannot break out."
  (:require [clojure.string :as str]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.search.export :as export]
            [dk.cst.corpus-probe.server.request :as request]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views :as views]
            [replicant.string :as replicant])
  (:import [java.io ByteArrayOutputStream]))

(defn ->transit
  "Encode `x` as a transit-JSON string."
  [x]
  (let [out (ByteArrayOutputStream.)]
    (transit/write (transit/writer out :json) x)
    (.toString out "UTF-8")))

(defn correct-quote-escaping
  "Work around a bug in Replicant's string renderer: it escapes `\"` as
  `&#39;` (an apostrophe) instead of `&#34;`, in both attributes and text.
  Real apostrophes are emitted as `&apos;`, so `&#39;` unambiguously marks a
  corrupted double quote and can be restored globally."
  ;; Still present in 2026.07.1, and fixed on Replicant's main branch but
  ;; in no release. Two conditions before dropping this, not one: the
  ;; escape has to emit `&#34;` for a double quote **and** still emit
  ;; `&apos;` for an apostrophe. If a release ever fixed the quote by
  ;; moving apostrophes to `&#39;`, this replacement would turn every
  ;; apostrophe in Danish corpus text into a double quote.
  ;; TODO: report upstream.
  [html]
  (str/replace html "&#39;" "&#34;"))

(defn script-safe
  "Escape `<` in transit text `s` as `\\u003c` so it cannot terminate the
  enclosing <script> element; JSON readers decode the escape back to `<`."
  [s]
  (str/replace s "<" "\\u003c"))

(defn document
  "The complete HTML document from `opts`: its `:lang`, its `:title`, the
  bypass link, the site header with its `:path` (the page being served,
  which its navigation marks) and navigation `:nav` (see
  dk.cst.corpus-probe.views/site-header), the rendered `:body` hiccup
  (the page's <main>) in #app, and the site footer. The `:payload` is
  the same view data as transit, embedded as the #bootstrap script the
  client takes over from.

  The masthead and the footer sit outside #app, so they are the document's
  banner and contentinfo rather than part of the main content. Each has
  a mount point of its own, because a routed navigation must re-render
  it or it goes stale: the masthead's links carry the current search,
  and the footer's words are in the UI language, which the language
  switch changes without reloading. The plain <div> around each scopes
  no landmark, so the <header> and <footer> inside are still the
  document's banner and contentinfo. Every page mounts the client, so
  every page routes: the client swaps those three regions rather than
  reloading, and the server keeps serving the same complete page for
  anything that does not run it. The document shell and the bootstrap
  script are emitted as strings rather than through Replicant, so the
  transit payload's double quotes are not mangled by the renderer bug
  (see `correct-quote-escaping`). The document language is the UI
  language; corpus text carries its own `lang`."
  [{:keys [lang path title body nav payload] :as opts}]
  (let [ui (i18n/->ui lang)]
    (str "<!DOCTYPE html>"
         "<html lang=\"" lang "\"><head>"
         "<meta charset=\"utf-8\">"
         "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
         (correct-quote-escaping
          (replicant/render
           [:meta {:name    "description"
                   :content (i18n/tr ui (str "Search CWB corpora and read "
                                             "KWIC concordances."))}]))
         (correct-quote-escaping (replicant/render [:title title]))
         "<link rel=\"stylesheet\" href=\"/css/reset.css\">"
         "<link rel=\"stylesheet\" href=\"/css/tokens.css\">"
         "<link rel=\"stylesheet\" href=\"/css/style.css\">"
         ;; what the chooser hides for a client to show again is shown
         ;; outright where no client can (see resources/public/css/noscript.css)
         "<noscript><link rel=\"stylesheet\" href=\"/css/noscript.css\"></noscript>"
         "</head><body>"
         (correct-quote-escaping (replicant/render (views/skip-link ui)))
         "<div id=\"masthead\">"
         (correct-quote-escaping
          (replicant/render (views/site-header ui path nav)))
         "</div>"
         "<div id=\"app\">"
         (correct-quote-escaping (replicant/render body))
         "</div>"
         "<div id=\"footer\">"
         (correct-quote-escaping (replicant/render (views/site-footer ui)))
         "</div>"
         (when payload
           (str "<script type=\"" url/transit-type "\" id=\"bootstrap\">"
                (script-safe payload)
                "</script>"))
         "<script defer src=\"/js/main.js\"></script>"
         "</body></html>")))

(defn transit-response
  "The view data `x` as transit, for the client router.

  It varies by the same things the document does, and is not stored: the
  same URL answers with a document or with data depending on the request,
  and a search is as fresh as the corpora behind it."
  [x]
  {:status  200
   :headers {"Content-Type"  (str url/transit-type "; charset=utf-8")
             "Vary"          "Accept, Accept-Language, Cookie"
             "Cache-Control" "no-store"}
   :body    (->transit x)})

(defn html-response
  "A complete HTML page response with `html` as its body.

  The page is served in the language its request asks for, so it varies by
  `Accept-Language` and says so; a shared cache would otherwise hand one
  reader's language to the next."
  [html]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"
             "Vary"         "Accept, Accept-Language, Cookie"}
   :body    html})

(def not-found
  "The 404 response to a path that names nothing."
  {:status 404 :body "not found"})

(def bad-request
  "The 400 response to a request that describes nothing to answer."
  {:status 400 :body "bad request"})

(defn resource-response
  "A 200 response serving classpath `resource` as `content-type`, uncached
  so dev assets always refetch."
  [content-type resource]
  {:status  200
   :headers {"Content-Type"  content-type
             "Cache-Control" "no-store"}
   :body    (slurp resource)})

(defn download-response
  "A 200 response serving `body` (text, or a function writing it to the
  response stream as it goes) in export `format` (a key of
  dk.cst.corpus-probe.search.export/formats) as a download named
  `filename`."
  [format filename body]
  {:status  200
   :headers {"Content-Type"        (:content-type (export/formats format))
             "Content-Disposition" (str "attachment; filename=\"" filename "."
                                        format "\"")}
   :body    body})

(defn export-failure
  "A 400 response explaining, corpus by corpus, why the search behind an
  export produced nothing to export, from the per-corpus `counts`, each
  error prepared for display (see dk.cst.corpus-probe.cwb/public-error)."
  [counts]
  {:status  400
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body    (->> counts
                 (map (fn [{:keys [corpus error]}]
                        (let [{:keys [type message]} (cwb/public-error error)]
                          (str corpus ": " (name (or type :cqp))
                               (some->> message (str "\n"))))))
                 (str/join "\n\n"))})

(defn export-response
  "The download of `rows` (a header and data rows of strings, see
  dk.cst.corpus-probe.search.export) rendered in `format` under
  `filename`, or the `export-failure` when `readable?` says no corpus of
  the `counts` could be searched, so a failed search never downloads as
  an empty file."
  [format filename readable? {:keys [counts]} rows]
  (if (some readable? counts)
    (download-response format filename ((:render (export/formats format)) rows))
    (export-failure counts)))

(defn shell-data
  "The parts of a page the masthead is built from, for `request` with
  search `params`: the `:path` being served, which its navigation marks as
  current and its language switch returns to, and the navigation `:nav`
  itself.

  They travel in the view data because the client re-renders the masthead,
  and the navigation depends on the search the reader is looking at."
  [request params]
  {:path (:uri request)
   :nav  (url/nav-hrefs params)})

(defn page-response
  "Answer `request` with the page `data` describes under `title`: as
  transit when the client router asked for it, else as the document its
  route renders from the same data (see dk.cst.corpus-probe.views/page).

  The masthead's own parts are merged in here, built for `nav-params`,
  the search its navigation carries. Every page is this one shape, and
  both representations come from one place, so the page the server paints
  and the page the client renders can never describe different things."
  ([request title data]
   (page-response request title data {}))
  ([request title data nav-params]
   (let [lang (:lang data)
         data (merge data (shell-data request nav-params))]
     (if (request/wants-transit? request)
       (transit-response (assoc data :title title))
       (html-response
        (document {:lang    lang
                   :path    (:path data)
                   :title   title
                   :nav     (:nav data)
                   :body    (views/page data)
                   :payload (->transit (assoc data :title title))}))))))
