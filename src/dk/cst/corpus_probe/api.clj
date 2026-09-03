(ns dk.cst.corpus-probe.api
  "HTTP routes and handlers: the server-rendered search page.

  Handlers close over the ctx map used by dk.cst.corpus-probe.search;
  responses are rendered from the shared .cljc views with Replicant's
  string renderer, so the coming client renders identical markup."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [dk.cst.corpus-probe.corpus :as corpus]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.views.page :as page]
            [replicant.string :as replicant])
  (:import [java.net URLEncoder]))

(defn html-response
  "Wrap `hiccup` as a complete HTML page response."
  [hiccup]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    (str "<!DOCTYPE html>"
                 (replicant/render
                  [:html {:lang "da"}
                   [:head
                    [:meta {:charset "utf-8"}]
                    [:meta {:name    "viewport"
                            :content "width=device-width, initial-scale=1"}]
                    [:title "corpus-probe"]
                    [:link {:rel "stylesheet" :href "/css/style.css"}]]
                   [:body hiccup]]))})

(defn query-string
  "Encode map `m` as a URL query string, skipping nil values."
  [m]
  (->> (remove (comp nil? val) m)
       (map (fn [[k v]]
              (str (name k) "=" (URLEncoder/encode (str v) "UTF-8"))))
       (str/join "&")))

(defn page-href
  "The URL of page `page` of the search described by `params`."
  [params page]
  (str "/?" (query-string (assoc params :page page))))

(defn ->cqp
  "The CQP query for `params`, compiling simple-mode input; nil when there
  is nothing to search for."
  [{:keys [q mode ci prefix suffix] :as params}]
  (when-not (str/blank? q)
    (if (= mode "simple")
      (query/simple->cqp q {:case-insensitive? (some? ci)
                            :prefix?           (some? prefix)
                            :suffix?           (some? suffix)})
      q)))

(defn search-page
  "Handle a search-page `request` against `ctx`: render the form, and when
  the query params describe a search, its KWIC result or CQP error."
  [ctx request]
  (let [params  (:query-params request)
        corpora (corpus/corpora ctx)
        corpus  (or (:corpus params) (some-> (first corpora) :id str/upper-case))
        cqp     (->cqp params)
        page-n  (or (some-> (:page params) parse-long) 0)
        outcome (when (and corpus cqp)
                  (try
                    {:result (search/kwic! ctx corpus cqp {:page page-n})}
                    (catch Exception e
                      {:error (or (:error (ex-data e))
                                  {:message (ex-message e)})})))
        pages   (when-let [{:keys [size page-size]} (:result outcome)]
                  (long (Math/ceil (/ size (double page-size)))))]
    (html-response
     (page/page {:corpora   corpora
                 :params    (assoc params :corpus corpus)
                 :result    (:result outcome)
                 :error     (:error outcome)
                 :prev-href (when (pos? page-n)
                              (page-href params (dec page-n)))
                 :next-href (when (and pages (< (inc page-n) pages))
                              (page-href params (inc page-n)))}))))

(defn stylesheet
  "Serve the bundled stylesheet."
  [_request]
  {:status  200
   :headers {"Content-Type" "text/css; charset=utf-8"}
   :body    (slurp (io/resource "public/css/style.css"))})

(defn routes
  "The route table, with handlers closed over `ctx`."
  [ctx]
  #{["/" :get (partial search-page ctx) :route-name ::search]
    ["/css/style.css" :get stylesheet :route-name ::stylesheet]})
