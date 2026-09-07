(ns dk.cst.corpus-probe.server.export
  "The TSV and CSV downloads of a search: its concordance, written corpus
  by corpus as each answers, and its frequency table whole."
  (:require [clojure.java.io :as io]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search.export :as export]
            [dk.cst.corpus-probe.search.frequency :as frequency]
            [dk.cst.corpus-probe.server.request :as request]
            [dk.cst.corpus-probe.server.response :as response]
            [dk.cst.corpus-probe.server.search :as search-server]))

(defn export-columns!
  "The annotation columns a concordance export over `corpora` via `ctx`
  has: the union of their positional attributes but word, which the
  match column is, and of their annotated s-attributes, each kind in
  the registry order of the first corpus reporting it; a corpus that
  cannot be read contributes none."
  [ctx corpora]
  (let [attrs (search-server/corpora-attrs! ctx corpus/attributes! corpora)]
    {:p-attrs      (vec (distinct (corpus/attr-names
                                   #(and (corpus/positional? %)
                                         (not= :word (:name %)))
                                   attrs)))
     :struct-attrs (vec (distinct (corpus/attr-names corpus/annotated-s-attr?
                                                     attrs)))}))

(defn serve-kwic-export
  "Handle a concordance export `request` against `ctx` in `format` (a
  key of dk.cst.corpus-probe.search.export/formats): the hits of the
  query in the selected corpora, the first
  dk.cst.corpus-probe.search.export/hit-limit of them in the requested
  sort, as a TSV or CSV download; 400 without a query, known corpora or
  a known format, or when no corpus could be searched.

  The corpora are exported one at a time (see
  dk.cst.corpus-probe.search/export-corpora!) and written as each
  answers. The first corpus to answer is waited for before the download
  starts, because a download once started can no longer be a 400: until
  one answers, the corpora that failed are collected, and if every one
  fails their reasons are the answer, as
  dk.cst.corpus-probe.server.response/export-failure gives them."
  [ctx request format]
  (let [{:keys [params known cqp opts]} (search-server/search-request!
                                         ctx request)]
    (if-not (and cqp (seq known) (export/formats format))
      response/bad-request
      (let [{:keys [line preamble]} (export/formats format)
            {:keys [p-attrs struct-attrs]} (export-columns! ctx known)
            header (export/kwic-header p-attrs struct-attrs)
            opts   (assoc opts
                          :sort    (:sort params)
                          :context (request/context-param (:context params))
                          :sample  (request/sample-param (:sample params)))
            ;; the exports are lazy: the corpora after the first answer
            ;; inside the stream, and the rows held at once stay within
            ;; the hit limit whichever corpora hold them
            [failed [head :as exports]]
            (split-with :error (search/export-corpora!
                                ctx known cqp (cwb/deadline ctx)
                                export/hit-limit opts))]
        (if (nil? head)
          (response/export-failure failed)
          (response/download-response
           format "kwic"
           (fn [out]
             (with-open [w (io/writer out :encoding "UTF-8")]
               (.write w (str preamble (line header)))
               (doseq [export exports
                       row    (export/kwic-rows p-attrs struct-attrs export)]
                 (.write w ^String (line row)))))))))))

(defn serve-frequencies-export
  "Handle a frequency table export `request` against `ctx` in `format`
  (a key of dk.cst.corpus-probe.search.export/formats): every row of the
  breakdown of the query (or of the whole corpora) by the `attr` param,
  against the `by` param when there is one, as a TSV or CSV download;
  400 without known corpora or a known format, or when no corpus could
  be counted."
  [ctx request format]
  (let [{:keys [params known cqp opts attr at docs] :as req}
        (assoc (search-server/read-request! ctx request) :view :frequencies)]
    (if-not (and (seq known) (export/formats format) (search-server/runs? req))
      response/bad-request
      (let [table (frequency/frequency-table!
                   ctx known (or cqp "") attr
                   (assoc opts
                          :at   at
                          :by   (request/by-param (:by params))
                          :docs docs
                          :sort (:sort params)))]
        (response/export-response format "frequencies" :tokens table
                                  (if (:by table)
                                    (export/crosstab-lines table)
                                    (export/frequency-lines table)))))))

(def export-file
  "What an export is named as under the search path: the view of the
  result as its name and the format as its extension, `kwic.tsv` (see
  dk.cst.corpus-probe.url/export)."
  #"(kwic|frequencies)\.(tsv|csv)")

(defn serve-export
  "Handle an export `request` against `ctx`: its `:file` path parameter
  names the view exported and the format it takes (see `export-file`);
  404 for a file that names neither."
  [ctx request]
  (let [[_ view format] (re-matches export-file
                                    (str (get-in request [:path-params :file])))]
    (case view
      "kwic"        (serve-kwic-export ctx request format)
      "frequencies" (serve-frequencies-export ctx request format)
      response/not-found)))
