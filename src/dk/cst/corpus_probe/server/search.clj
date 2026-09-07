(ns dk.cst.corpus-probe.server.search
  "The search page and the data behind it: what a search request asks,
  the search run in the view asked for (a concordance or a frequency
  table over the selected corpora), the state of the form and the links
  out of the result, and the three endpoints the client asks on its own:
  the metadata filters a corpus selection offers, one hit with wider
  context, and the count of a search still being counted when its page
  was served."
  (:require [dk.cst.corpus-probe.cqp :as cqp]
            [dk.cst.corpus-probe.cwb :as cwb]
            [dk.cst.corpus-probe.cwb.command :as command]
            [dk.cst.corpus-probe.cwb.corpus :as corpus]
            [dk.cst.corpus-probe.cwb.registry :as registry]
            [dk.cst.corpus-probe.cwb.tools :as tools]
            [dk.cst.corpus-probe.docs :as docs]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.query.mode :as mode]
            [dk.cst.corpus-probe.search :as search]
            [dk.cst.corpus-probe.search.batch :as batch]
            [dk.cst.corpus-probe.search.export :as export]
            [dk.cst.corpus-probe.search.frequency :as frequency]
            [dk.cst.corpus-probe.server.request :as request]
            [dk.cst.corpus-probe.server.response :as response]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views :as views]
            [dk.cst.corpus-probe.views.frequency :as frequency-views]))

(defn selected-corpora!
  "The corpus names `params` asks for against the registry `entries` via
  `ctx`: those it names, or every readable corpus when it names none and
  the selection is not one the reader made (see
  dk.cst.corpus-probe.cwb.corpus/readable-corpora!).

  The search form submits a `scope` param alongside its checkboxes, so an
  empty selection a reader ticked their way to is answered with the no
  corpus error rather than silently widened to the whole registry."
  [ctx entries params]
  (let [named (url/corpora-param (:corpus params))]
    (if (or (seq named) (contains? params :scope))
      named
      (corpus/readable-corpora! ctx entries))))

(defn search-request!
  "What `request` asks of `ctx`: its scalar query `:params`, the
  registry's `:entries`, the corpus names `:named` in the params, the
  names `:selected` to search, those split into the `:known` and the
  `:unknown` (see dk.cst.corpus-probe.cwb.corpus/split-known), what
  `:arrived` with the form, a change of its mode allowed for (see
  dk.cst.corpus-probe.query/arrived), the `:cqp` the query that runs
  compiles to (see dk.cst.corpus-probe.query/->cqp) and the `:opts` every
  search of it takes: its metadata :filter (see
  dk.cst.corpus-probe.server.request/filter-params) and the :patterns
  beside it (see dk.cst.corpus-probe.server.request/pattern-params), the
  unit of text it is kept :within (see dk.cst.corpus-probe.query/within),
  the :subset of its hits kept (see
  dk.cst.corpus-probe.server.request/subset-param) and the word its hits
  are :near (see dk.cst.corpus-probe.server.request/near-param). A request
  naming no corpus searches every readable one (see `selected-corpora!`).

  Every handler that answers a search starts from this."
  [ctx request]
  (let [params   (request/scalar-params (:query-params request))
        arrived  (query/arrived params)
        query    (:query arrived)
        entries  (registry/entries ctx)
        selected (selected-corpora! ctx entries params)
        [known unknown] (corpus/split-known entries selected)]
    {:params   params
     :arrived  arrived
     :entries  entries
     :named    (url/corpora-param (:corpus params))
     :selected selected
     :known    known
     :unknown  unknown
     :cqp      (query/->cqp query)
     :opts     {:filter   (request/filter-params params)
                :patterns (request/pattern-params params)
                :within   (query/within query)
                :subset   (request/subset-param params)
                :near     (request/near-param (:near params)
                                              (:distance params))}}))

(defn read-request!
  "What the search page `request` asks of `ctx`: the search it describes
  (see `search-request!`) with what the page reads beside it: the `:view`
  of the result (see dk.cst.corpus-probe.server.request/view-param), the
  `:attr` and `:at` a frequency table groups by and whether it counts
  `:docs`, the `:page` of a concordance, the `:lang` the page is served
  in, and whether the client asked for the data alone, `:transit?` (see
  dk.cst.corpus-probe.server.request/wants-transit?)."
  [ctx request]
  (let [{:keys [params] :as req} (search-request! ctx request)]
    (assoc req
           :view     (request/view-param (:view params))
           :attr     (request/attr-param (:attr params))
           :at       (request/position-param (:at params))
           :docs     (some? (:docs params))
           :page     (request/page-param (:page params))
           :lang     (request/request-language request)
           :transit? (request/wants-transit? request))))

(defn runs?
  "True when the search `req` (see `read-request!`) runs in its `:view`:
  it carries a query, or it is the frequency view of the corpora whole,
  which a blank query counts, unless the query is blank only because a
  change of mode could not keep it (see
  dk.cst.corpus-probe.query/arrived), when the form is shown and nothing
  runs."
  [{:keys [view params cqp known unknown]}]
  (boolean (or cqp
               (and (= :frequencies view)
                    (not (mode/unread-query? params))
                    (or (seq known) (seq unknown))))))

(defn shown-params
  "The params the search page for `req` (see `read-request!`) shows in
  its form and cites in its links: what arrived, less the form's own
  query keys, with the query the form holds in the form's own spelling
  (see dk.cst.corpus-probe.query/->params) over it, so that a control the
  mode does not read keeps what it carried, as memory; the corpora
  searched, or only those named when nothing runs (see `runs?`), since a
  reader arriving at the form starts with none selected; and the
  grouping of the frequency view. The mode is the form's (see
  dk.cst.corpus-probe.query.mode/form-of), which the radios read and no
  URL carries."
  [{:keys [params arrived selected named] :as req}]
  (let [{:keys [form held]} arrived]
    (-> (apply dissoc params (mode/read-keys form params))
        (merge (query/->params form held))
        (assoc :mode   (mode/form-of form)
               :corpus (if (runs? req) selected named)
               :attr   (request/attr-param (:attr params))
               :at     (request/position-param (:at params))))))

(defn citation!
  "The URL params the search page for `req` (see `read-request!`) cites
  (see `shown-params` and dk.cst.corpus-probe.url/canonical), against
  the corpora `ctx` can read."
  [ctx {:keys [entries] :as req}]
  (url/canonical (shown-params req)
                 (set (corpus/readable-corpora! ctx entries))))

(defn corpora-attrs!
  "The attribute descriptions `f` (a function of `ctx` and a corpus name)
  reports for each of `corpora`, read in parallel and concatenated in
  corpus order; a corpus that cannot be read contributes none."
  [ctx f corpora]
  (->> (cwb/pmap-n (cwb/parallelism ctx)
                   #(cwb/attempt % (fn [] (vec (f ctx %))))
                   corpora)
       ;; a corpus that failed is the error map `attempt` gives it, one
       ;; that answered a vector of descriptions
       (remove :error)
       (apply concat)))

(defn attr-options!
  "The attribute descriptions ({:type :name}) offered for grouping the
  `corpora` via `ctx`: their union, positional attributes first.

  Each kind keeps the registry order of the first corpus reporting it; a
  corpus that cannot be read contributes none. Falls back to word, the one
  attribute every corpus has. Every attribute is offered whatever the
  query: a structural one cannot table a whole corpus, and that request
  is then rejected with its reason, so the form still shows what was
  asked."
  [ctx corpora]
  (let [attrs (->> (corpora-attrs! ctx frequency/groupable-attrs! corpora)
                   (map #(select-keys % [:type :name]))
                   (distinct)
                   (sort-by (comp {:positional 0 :structural 1} :type))
                   (vec))]
    (if (seq attrs) attrs [{:type :positional :name :word}])))

(defn sort-options
  "The sort modes offered over corpora whose positional attributes are
  `attrs` (keywords): the fixed modes (see
  dk.cst.corpus-probe.cwb.command/sort-modes), then a sort by each
  attribute but word, which the match sort already is."
  [attrs]
  (into (mapv first command/sort-modes)
        (comp (remove #{:word}) (map name))
        attrs))

(defn filter-controls!
  "The metadata filter controls of the search form over the `known`
  corpora via `ctx`: the filters they offer (see
  dk.cst.corpus-probe.search.frequency/filter-options!) plus the
  `:selected` values of `params` (see
  dk.cst.corpus-probe.server.request/filter-params) and what its pattern
  and range fields hold (see
  dk.cst.corpus-probe.server.request/pattern-fields)."
  [ctx known params]
  (merge (frequency/filter-options! ctx known)
         {:selected (request/filter-params params)}
         (request/pattern-fields params)))

(defn value-lists!
  "The values of each positional attribute among `attrs` (keywords) that
  every one of `corpora` via `ctx` can list (see
  dk.cst.corpus-probe.cwb.tools/attribute-values!): attribute to its
  values over all of them, collated. An attribute one corpus cannot list,
  or lacks, has no entry, since a list missing part of what a reader may
  search for would mislead. The value fields of the extended search
  offer them as suggestions."
  [ctx corpora attrs]
  (let [collator (cwb/->collator ctx)
        lists    (cwb/pmap-n (cwb/parallelism ctx)
                             (fn [corpus]
                               (into {}
                                     (for [attr attrs]
                                       [attr (try (tools/attribute-values!
                                                   ctx corpus attr)
                                                  (catch Exception _ nil))])))
                             corpora)]
    (into {}
          (for [attr attrs
                :let [values (map #(get % attr) lists)]
                :when (and (seq values) (every? some? values))]
            [attr (vec (sort collator (distinct (apply concat values))))]))))

(defn unknown-counts
  "The count entries reporting the `unknown` corpus names as such."
  [unknown]
  (mapv (fn [corpus] {:corpus corpus :error {:type :unknown-corpus}})
        unknown))

(defn public-counts
  "The per-corpus counts of concordance `result` with each error prepared
  for display by dk.cst.corpus-probe.cwb/public-error."
  [result]
  (update result :counts
          (partial mapv #(cond-> % (:error %)
                           (update :error cwb/public-error)))))

(defn public-result
  "`result` with its counts prepared for display (see `public-counts`)
  and its :pages, once every corpus is counted."
  [result]
  (cond-> (public-counts result)
    (not (:remaining result)) (assoc :pages (url/page-count result))))

(defn search-outcome!
  "Search the `known` corpora for `cqp` via `ctx` with `opts` (the :page,
  :sort, :context, :sample, :filter, :near, :within and :incremental? of
  dk.cst.corpus-probe.search/concordance!): {:result <concordance with
  its :pages, once every corpus is counted>}, the `unknown` corpus names
  reported among its counts, or {:error ...} when no corpus was selected
  at all. Per-corpus errors travel inside the result."
  [ctx known unknown cqp opts]
  (if (and (empty? known) (empty? unknown))
    {:error {:type :no-corpus}}
    {:result (-> (search/concordance! ctx known cqp opts)
                 (update :counts into (unknown-counts unknown))
                 (public-result))}))

(defn linked-rows
  "The frequency `result` with a dk.cst.corpus-probe.url/subset-href on
  each of the rows the table shows (its first
  dk.cst.corpus-probe.views.frequency/row-limit), for the search
  described by `params`: the rows past those go unlinked, since the
  table does not show them and an export reads no links."
  [params {:keys [attr at] :as result}]
  (update result :rows
          (fn [rows]
            (into (mapv #(assoc % :href (url/subset-href params attr at
                                                         (:value %)))
                        (take frequency-views/row-limit rows))
                  (drop frequency-views/row-limit rows)))))

(defn frequency-outcome!
  "Table the `known` corpora for `cqp` (nil for the whole corpora) by
  `attr` via `ctx` with `opts` (the :at, :by, :docs, :filter, :within,
  :subset and :near of
  dk.cst.corpus-probe.search.frequency/frequency-table!): {:result ...},
  the `unknown` corpus names reported among its counts, or {:error ...}
  when no corpus was selected at all. Per-corpus errors travel inside
  the result."
  [ctx known unknown cqp attr opts]
  (if (and (empty? known) (empty? unknown))
    {:error {:type :no-corpus}}
    {:result (-> (frequency/frequency-table! ctx known (or cqp "") attr opts)
                 (update :counts into (unknown-counts unknown))
                 (public-counts))}))

(defn run-view!
  "The outcome of the search `req` (see `read-request!`) asks of `ctx`,
  in its `:view`: {:result ...} or {:error ...} from `frequency-outcome!` or
  `search-outcome!`, with the rows of a frequency table linked (see
  `linked-rows`); nil when nothing runs (see `runs?`)."
  [ctx {:keys [view params known unknown cqp opts attr at docs page transit?]
        :as   req}]
  (cond
    (and (= :frequencies view) (runs? req))
    (-> (frequency-outcome! ctx known unknown cqp attr
                            (assoc opts
                                   :at   at
                                   :docs docs
                                   :by   (request/by-param (:by params))
                                   ;; the concordance saved its result
                                   ;; under this
                                   :sort (:sort params)))
        (update :result #(some->> % (linked-rows params))))

    cqp
    (search-outcome! ctx known unknown cqp
                     (assoc opts
                            :page    page
                            :sort    (:sort params)
                            :context (request/context-param
                                      (:context params))
                            :sample  (request/sample-param
                                      (:sample params))
                            ;; a document waits for the count; the client
                            ;; asks for it itself (see `serve-counts`)
                            :incremental? transit?))))

(defn form-data!
  "The state of the search form for `req` (see `read-request!`) via
  `ctx`, over the attribute descriptions `attrs` offered for its corpora
  (see `attr-options!`): the corpus chooser's `:folders`, the metadata
  `:filter-controls` (see `filter-controls!`), the `:search-attrs` a
  simple search may match and a concordance sorts by (the positional
  ones), the `:tokens` of the extended form (see
  dk.cst.corpus-probe.query/form-rows), the `:value-lists` its fields
  suggest (see `value-lists!`), the `:params` that fill the form (see
  `shown-params`), the same as `:asked` for the result to read, and what
  a change of mode could not keep as `:switch`, its `:loss` and the
  `:unread` params, for the form's status line (see
  dk.cst.corpus-probe.views.search/switch-notice).

  `:params` names every corpus searched, or only what the URL named when
  nothing was searched: a reader arriving at the form starts with no
  corpus selected, while a URL naming no corpus still searches every
  readable one and shows them all."
  [ctx {:keys [params arrived entries known] :as req} attrs]
  (let [{:keys [form held]} arrived
        shown   (shown-params req)
        p-attrs (corpus/attr-names corpus/positional? attrs)]
    {:folders         (corpus/corpus-tree! ctx entries)
     :filter-controls (filter-controls! ctx known params)
     :search-attrs    p-attrs
     :tokens          (query/form-rows (when (= "extended" form) held))
     :switch          (select-keys arrived [:loss :unread])
     :value-lists     (value-lists! ctx known p-attrs)
     :params          shown
     ;; the same, frozen: the client's form moves on from `:params` at a
     ;; change of mode, while the result still answers what was asked
     :asked           shown}))

(defn result-data
  "The outcome of the search `req` (see `read-request!`) ran, for the
  results region: its `:result` or `:error` from `outcome` (see
  `run-view!`), the language of each corpus searched as `:langs`, and
  the controls its view offers: the `:attrs` and `:positions` a
  frequency table groups by, from the attribute descriptions `attrs`
  (see `attr-options!`), or the `:sort-modes` of a concordance (see
  `sort-options`) and the `:export-limit` an export of it holds."
  [{:keys [view entries selected] :as req} attrs outcome]
  (cond-> {:result (:result outcome)
           :error  (:error outcome)
           :langs  (into {}
                         (map (juxt identity #(corpus/corpus-lang entries %)))
                         selected)}
    (= :frequencies view)
    (assoc :attrs     attrs
           :positions command/positions)

    (not= :frequencies view)
    (assoc :sort-modes   (sort-options (corpus/attr-names corpus/positional?
                                                          attrs))
           :export-limit export/hit-limit)))

(defn links
  "The links out of the search page for `req` (see `read-request!`),
  cited as `cited` (see `citation!`): to each view of the result as
  `:view-hrefs`, to its exports as `:export-hrefs` once `outcome` holds
  a result, and, for a concordance, to the pages before and after as
  `:prev-href` and `:next-href`."
  [{:keys [view params page attr at] :as req} cited outcome]
  (let [result  (:result outcome)
        exports (fn [view params]
                  (when result
                    (url/export-hrefs view url/export-formats params)))]
    (if (= :frequencies view)
      {:view-hrefs   (url/view-hrefs cited)
       :export-hrefs (exports :frequencies
                              (assoc (url/search-params cited)
                                     :attr attr
                                     :at   at
                                     :by   (:by params)
                                     :docs (:docs params)))}
      (merge {:view-hrefs   (url/view-hrefs cited)
              :export-hrefs (exports :kwic
                                     (assoc (url/search-params cited)
                                            :sort    (:sort params)
                                            :context (:context params)))}
             (url/page-hrefs cited page result)))))

(defn search-view-data
  "The data dk.cst.corpus-probe.views/search-page renders one search
  page from, for `request` against `ctx`, or for the search `req` it
  reads (see `read-request!`) with the citation `cited` the page carries
  (see `citation!`): the state of the form (see `form-data!`), the
  outcome of the search when the params describe one (see `run-view!`
  and `result-data`), and the links out of it (see `links`).

  One search, two views: the `view` param decides whether its hits are
  listed as a concordance or counted as a frequency table, and each view
  contributes only the controls and links it has (a sort and pagination
  for the concordance, a grouping for the table). Links are built from
  `:cited`, the params as the URL cites them; `:params` fills the form.

  The same map is embedded as transit for the client to take over from,
  so it holds corpus overviews only: the full registry maps carry
  absolute server paths and stay here."
  ([ctx request]
   (let [req (read-request! ctx request)]
     (search-view-data ctx req (citation! ctx req))))
  ([ctx {:keys [lang view known] :as req} cited]
   (let [outcome (run-view! ctx req)
         attrs   (attr-options! ctx known)]
     (merge {:lang lang :view view :cited cited}
            (form-data! ctx req attrs)
            (result-data req attrs outcome)
            (links req cited outcome)))))

(defn serve-search
  "Handle a search-page `request` against `ctx`: render the form, and when
  the query params describe a search, its concordance or the reason there
  is none, else the search help (see dk.cst.corpus-probe.docs) where
  the results will be.

  A document asked for by a query string that is not the search's
  citation (see `citation!`) is answered with a redirect to it, so that
  the address bar of a submit without the client shows the one URL the
  search has, as a routed submit does (see
  dk.cst.corpus-probe.client.router/submit-query-string). Not for a form
  submitted
  with its mode changed, whose citation is what the form holds rather
  than what it was given, and which runs nothing until sent again.

  `:cited` goes to the masthead's navigation and not to the client, which
  does not read it."
  [ctx request]
  (let [req   (read-request! ctx request)
        cited (citation! ctx req)]
    (if (and (not (:transit? req))
             (nil? (get-in req [:arrived :from]))
             (not= (or (:query-string request) "")
                   (url/query-string cited)))
      {:status  303
       :headers {"Location" (url/results-href cited)}}
      (let [{:keys [result error] :as data} (search-view-data ctx req cited)
            data (cond-> (assoc (dissoc data :cited) :route :search)
                   (not (or result error))
                   (assoc :help (docs/document
                                 "help" (request/request-languages request))))]
        (response/page-response request (views/title data) data cited)))))

(defn serve-filters
  "Answer the metadata filters the corpora named in `request` offer via
  `ctx`, as transit, so the client can refresh the filter fieldset when
  the corpus selection changes without submitting a search.

  Only names the registry has reach CQP (see
  dk.cst.corpus-probe.cwb.corpus/split-known); an unknown one simply
  contributes nothing, since nothing is being searched here.

  The values a reader has chosen are not answered: those are the reader's
  and the client is already holding them. An attribute list that no
  longer offers a chosen value leaves that value where it is (see
  dk.cst.corpus-probe.views.search.filter/filter-fieldset), so narrowing
  the corpora never quietly drops part of a filter.

  The per-corpus half of this is cached against each registry file (see
  dk.cst.corpus-probe.cwb.tools/annotation-values!), so a repeat
  selection costs the merge and the collated sort rather than a CQP
  round trip."
  [ctx request]
  (let [entries   (registry/entries ctx)
        named     (url/corpora-param (:corpus (:query-params request)))
        [known _] (corpus/split-known entries named)]
    (response/transit-response (frequency/filter-options! ctx known))))

(def expanded-context
  "Context width, in tokens each side, for an expanded hit."
  50)

(defn serve-context
  "Answer the hit at the corpus position `request` names via `ctx` with
  wider context, as transit, for the client's context expansion.

  Rejects an invalid corpus or non-integer `cpos`/`matchend`, since those
  are interpolated into a CQP query (as validated integers) outside the
  sandbox."
  [ctx request]
  (let [{:keys [corpus cpos matchend]} (:query-params request)
        cpos*     (parse-long (str cpos))
        matchend* (parse-long (str matchend))]
    (if-not (and (cqp/corpus-name? corpus) cpos* matchend*)
      response/bad-request
      (try
        (let [q      (command/position-query cpos* matchend*)
              ;; one hit at a position nothing will ask for again, so
              ;; saving it would only fill the cache (see
              ;; dk.cst.corpus-probe.search.cache)
              result (search/kwic! ctx corpus q {:context      expanded-context
                                                 :rows         [0 0]
                                                 :struct-attrs []
                                                 :cache?       false})]
          (if-let [hit (first (:hits result))]
            (response/transit-response hit)
            ;; a position that matches nothing (out of range) is not found
            response/not-found))
        (catch Exception _
          response/not-found)))))

(defn serve-counts
  "Answer the count of the search `request` describes against `ctx`, as
  transit: the per-corpus `:counts`, the `:size` and `:pages` of the
  whole result, the `:prev-href` and `:next-href` of the page the
  request names and the document `:title`, everything about a page that
  the count decides. For the client, whose page arrived while its
  corpora were still being counted (see `run-view!`).

  Asked with the page's own params, so it counts the question the page
  answered. The corpora the page showed were remembered as it was filled
  (see dk.cst.corpus-probe.search/remember-size!), so only the rest cost
  a query, and each count is remembered for the next page. A request
  describing no search is refused."
  [ctx request]
  (let [{:keys [params selected known unknown cqp opts page lang] :as req}
        (read-request! ctx request)]
    (if-not cqp
      response/bad-request
      (let [counts (-> (search/corpus-sizes!
                        ctx known cqp (cwb/deadline ctx)
                        (assoc opts :sample (request/sample-param
                                             (:sample params))))
                       (into (unknown-counts unknown)))
            result (public-result
                    (assoc batch/page-defaults
                           :page   page
                           :counts counts
                           :size   (reduce + (keep :size counts))))
            ;; titled as the page is, the filter it was kept within named
            ;; beside the count (see dk.cst.corpus-probe.views/search-title)
            title  (views/title
                    {:route  :search
                     :lang   lang
                     :view   :kwic
                     :params (assoc params :corpus selected)
                     :result (merge result
                                    (select-keys opts [:filter :patterns]))})]
        (response/transit-response
         (merge (select-keys result [:counts :size :pages])
                (url/page-hrefs (citation! ctx req) page result)
                {:title title}))))))
