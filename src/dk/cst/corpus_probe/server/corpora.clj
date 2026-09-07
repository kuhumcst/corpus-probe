(ns dk.cst.corpus-probe.server.corpora
  "The corpus pages: the index of every registry corpus grouped by the
  configured folder tree, the page of one corpus with its statistics and
  info text, and the reading page of one of its texts."
  (:require [clojure.string :as str]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.registry :as registry]
            [dk.cst.corpus-probe.cwb.tools :as tools]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.server.request :as request]
            [dk.cst.corpus-probe.server.response :as response]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views :as views]))

(defn serve-corpora
  "Handle the corpus index `request` against `ctx`: every registry corpus,
  summarized and grouped by the configured folder tree."
  [ctx request]
  (let [data {:route :corpora
              :lang  (request/request-language request)
              :data  {:folders (corpus/corpus-tree! ctx
                                                    (registry/entries ctx))}}]
    (response/page-response request (views/title data) data)))

(defn serve-corpus
  "Handle a corpus info page `request` against `ctx`, rendering the corpus
  named by the :id path parameter (case-insensitively, see
  dk.cst.corpus-probe.cwb.registry/entry-of); 404 when it is not a
  registry corpus."
  [ctx request]
  (let [lang   (request/request-language request)
        corpus (str/upper-case (str (get-in request [:path-params :id])))
        entry  (registry/entry-of ctx corpus)]
    (if-not entry
      response/not-found
      (let [outcome (cwb/attempt corpus
                                 (fn []
                                   {:stats (tools/describe-corpus! ctx corpus)
                                    :info  (corpus/info! ctx corpus)})
                                 (fn [e] {:phantom? (corpus/phantom? e)}))
            data    {:route :corpus
                     :lang  lang
                     ;; the corpus's own language lives in :data, where
                     ;; info-page reads it; the UI language is the page's
                     :data  (assoc outcome
                                   :corpus corpus
                                   :title  (not-empty (:name entry))
                                   :lang   (registry/language entry))}]
        (response/page-response request (views/title data) data)))))

(defn text-response
  "The reading page of `corpus` (its registry map `entry`) for `request`,
  from the `outcome` of reading its text (see
  dk.cst.corpus-probe.search/text!, the error prepared for display) with
  the hit from corpus position `cpos` to `end` marked (see
  dk.cst.corpus-probe.views.corpus/reading-page)."
  [request corpus entry outcome [cpos end]]
  (let [data {:route :text
              :lang  (request/request-language request)
              ;; the corpus's own language lives in :data, as on the
              ;; corpus page; the UI language is the page's
              :data  (cond-> (assoc outcome
                                    :corpus corpus
                                    :hit    [cpos end]
                                    :lang   (registry/language entry))
                       (:error outcome)
                       (update :error cwb/public-error))}]
    (response/page-response request (views/title data) data)))

(defn serve-text
  "Handle a reading page `request` against `ctx`: the text of the corpus
  named by the :id path parameter that holds the corpus position of the
  `cpos` query param, with the hit from there to the `matchend` param
  (`cpos` itself when absent) marked (see `text-response`); 404 when the
  corpus is not a registry corpus, the position is not a number or no
  text holds it. Without any position the reader is sent to the corpus
  page, there being no text to pick without one. A corpus that marks no
  texts, or a CQP failure, is a page saying so."
  [ctx request]
  (let [corpus (str/upper-case (str (get-in request [:path-params :id])))
        entry  (registry/entry-of ctx corpus)
        {:keys [cpos matchend]} (:query-params request)
        cpos*  (parse-long (str cpos))]
    (cond
      (nil? entry)
      response/not-found

      (str/blank? (str cpos))
      {:status 303 :headers {"Location" (url/corpus corpus)} :body ""}

      (nil? cpos*)
      response/not-found

      :else
      (let [outcome (cwb/attempt corpus #(search/text! ctx corpus cpos*))
            ;; a hit ends where it starts unless told otherwise, and
            ;; never before it starts
            end     (max cpos* (or (some-> matchend str parse-long) cpos*))]
        (if (nil? outcome)
          response/not-found
          (text-response request corpus entry outcome [cpos* end]))))))
