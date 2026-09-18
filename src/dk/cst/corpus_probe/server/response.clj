(ns dk.cst.corpus-probe.server.response
  "The responses the handlers answer with: a page as the HTML document
  its route renders, or as the same view data in transit, the downloads,
  the classpath files and the plain refusals.

  Hostile corpus content survives the round trip because each channel is
  protected: `correct-quote-escaping` fixes the SSR body and
  `script-safe` escapes the `<` that transit passes through verbatim."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
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
  "Restore the double quotes Replicant's string renderer escapes as
  `&#39;`, an apostrophe, in `html`; real apostrophes come out as
  `&apos;`, so the mark is unambiguous."
  ;; still in 2026.07.1, fixed upstream but unreleased. Drop this only
  ;; once a release emits &#34; for a quote AND still &apos; for an
  ;; apostrophe: one moving apostrophes to &#39; would make this corrupt
  ;; every apostrophe in the corpus text.
  ;; TODO: report upstream
  [html]
  (str/replace html "&#39;" "&#34;"))

(defn script-safe
  "Escape `<` in transit text `s` as `\\u003c` so it cannot terminate the
  enclosing <script> element; JSON readers decode the escape back to `<`."
  [s]
  (str/replace s "<" "\\u003c"))

(defn render
  "The hiccup `x` as HTML, its double quotes restored (see
  `correct-quote-escaping`)."
  [x]
  (correct-quote-escaping (replicant/render x)))

(defn document
  "The complete HTML document from `opts`: its `:lang` (the UI language;
  corpus text carries its own), its `:title`, the bypass link, the site
  header with its `:path` and navigation `:nav`, the rendered `:body`
  hiccup in #app, and the site footer. The `:payload` is the same view
  data as transit, embedded as the #bootstrap script the client takes
  over from."
  [{:keys [lang path title body nav payload] :as opts}]
  (let [ui (i18n/->ui lang)]
    (str "<!DOCTYPE html>"
         "<html lang=\"" lang "\"><head>"
         "<meta charset=\"utf-8\">"
         "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
         (render [:meta {:name    "description"
                         :content (i18n/tr ui (str "Search CWB corpora and "
                                                   "read KWIC concordances."))}])
         (render [:title title])
         "<link rel=\"stylesheet\" href=\"/css/reset.css\">"
         "<link rel=\"stylesheet\" href=\"/css/tokens.css\">"
         "<link rel=\"stylesheet\" href=\"/css/style.css\">"
         ;; what the chooser hides for a client to show again is shown
         ;; outright where no client can (see public/css/noscript.css)
         "<noscript><link rel=\"stylesheet\" href=\"/css/noscript.css\"></noscript>"
         "</head><body>"
         (render (views/skip-link ui))
         ;; the masthead, #app and the footer each get a mount point of
         ;; their own: a routed navigation must re-render all three or
         ;; they go stale. The plain <div> scopes no landmark, so the
         ;; <header> and <footer> inside are still banner and contentinfo
         "<div id=\"masthead\">" (render (views/site-header ui path nav)) "</div>"
         "<div id=\"app\">" (render body) "</div>"
         "<div id=\"footer\">" (render (views/site-footer ui)) "</div>"
         ;; a string rather than hiccup: Replicant's renderer would mangle
         ;; the payload's double quotes (see `correct-quote-escaping`)
         (when payload
           (str "<script type=\"" url/transit-type "\" id=\"bootstrap\">"
                (script-safe payload)
                "</script>"))
         "<script defer src=\"/js/main.js\"></script>"
         "</body></html>")))

(defn transit-response
  "The view data `x` as transit, for the client router."
  [x]
  ;; varies as the document does, and is not stored: one URL answers with
  ;; a document or data by the request, and a search is only as fresh as
  ;; the corpora behind it
  {:status  200
   :headers {"Content-Type"  (str url/transit-type "; charset=utf-8")
             "Vary"          "Accept, Accept-Language, Cookie"
             "Cache-Control" "no-store"}
   :body    (->transit x)})

(defn html-response
  "A complete HTML page response with `html` as its body."
  [html]
  ;; served in the language the request asks for, so it says it varies by
  ;; it, or a shared cache would hand one reader's language to the next
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
  "A 200 response serving classpath `resource` as `content-type`, kept by
  the reader's browser for `seconds` or, with none, not at all, so that a
  dev asset always refetches."
  [content-type resource seconds]
  ;; the bytes rather than the text: a font read as text is corrupted
  {:status  200
   :headers {"Content-Type"  content-type
             "Cache-Control" (if seconds
                               (str "public, max-age=" seconds)
                               "no-store")}
   :body    (io/input-stream resource)})

(defn download-response
  "A 200 response serving `body` (text, or a function writing it to the
  response stream as it goes) in export `format` as a download named
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
    (download-response format filename (export/render format rows))
    (export-failure counts)))

(defn shell-data
  "The parts of a page the masthead is built from, for `request` with
  search `params`: the `:path` being served, which its navigation marks
  as current and its language switch returns to, and the `:nav` itself."
  [request params]
  ;; in the view data because the client re-renders the masthead, and
  ;; the navigation depends on the search being looked at
  {:path (:uri request)
   :nav  (url/nav-hrefs params)})

(defn page-response
  "Answer `request` with the page `data` describes under `title`, the
  masthead built for `nav-params`: as transit when the client router
  asked for it, else as the document its route renders from the same
  data, so the two can never describe different things."
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
