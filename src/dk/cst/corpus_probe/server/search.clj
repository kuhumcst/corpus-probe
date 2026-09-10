(ns dk.cst.corpus-probe.server.search
  "The search page and the data behind it: what a search request asks,
  the search run in the view asked for, the state of the form and the
  links out of the result, and the two endpoints the client asks on its
  own: the metadata filters a corpus selection offers, and the count of
  a search still being counted when its page was served."
  (:require [dk.cst.corpus-probe.cwb :as cwb]
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
            [dk.cst.corpus-probe.settings :as settings]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views :as views]
            [dk.cst.corpus-probe.views.frequency :as frequency-views]))

(defn selected-corpora
  "The corpus names `params` asks for out of the `selectable` ones: those
  it names, or all of them when it names none and the selection is not
  one the reader made.

  The form submits a `scope` param alongside its checkboxes, so an empty
  selection a reader ticked their way to is answered with the no-corpus
  error rather than silently widened to the whole registry."
  [selectable params]
  (let [named (url/corpora-param (:corpus params))]
    (if (or (seq named) (contains? params :scope))
      named
      (vec selectable))))

(defn search-request!
  "What `request` asks of `ctx`: its scalar query `:params`, the
  registry's `:entries`, the `:selectable` corpora, the corpus names
  `:named`, `:selected`, `:known` and `:unknown`, what `:arrived` with
  the form, the `:cqp` the query compiles to and the `:opts` every
  search of it takes. A request naming no corpus searches every readable
  one (see `selected-corpora`).

  Every handler that answers a search starts from this."
  [ctx request]
  (let [entries (registry/entries ctx)
        ;; read once and carried: the citation, the selection and the
        ;; stored settings all measure against the same set, and reading
        ;; it runs cqp over every corpus of the registry
        selectable      (corpus/readable-corpora! ctx entries)
        ;; the stored settings name every corpus with a scope, which
        ;; nothing past this reads
        params          (url/with-every-corpus
                          (request/scalar-params (:query-params request))
                          selectable)
        arrived         (query/arrived params)
        query           (:query arrived)
        selected        (selected-corpora selectable params)
        [known unknown] (corpus/split-known entries selected)]
    {:params     params
     :arrived    arrived
     :entries    entries
     :selectable selectable
     :named      (url/corpora-param (:corpus params))
     :selected   selected
     :known      known
     :unknown    unknown
     :cqp        (query/->cqp query)
     :opts       {:filter   (request/filter-params params)
                  :patterns (request/pattern-params params)
                  :ranges   (request/range-params params)
                  :within   (query/within query)
                  :subset   (request/subset-param params)
                  :near     (request/near-param (:near params)
                                                (:distance params))}}))

(defn read-request!
  "What the search page `request` asks of `ctx`: the search it describes
  (see `search-request!`) plus what the page reads beside it: the `:view`
  of the result, the `:attr` and `:at` a frequency table groups by and
  whether it counts `:docs`, the `:page` of a concordance, the `:lang`
  the page is served in, the settings its reader `:stored` and whether
  the params were `:seeded?` from them, whether it stores them as they
  change (`:autosave?`), and whether the client asked for the data
  alone, `:transit?`."
  [ctx request]
  (let [cookie  (request/stored-settings request)
        stored  (settings/params cookie)
        ;; the stored settings stand in only for a request that asks for
        ;; none: a URL that asks anything is read as it stands, or a link
        ;; to a result would find different hits for the reader who
        ;; stored a selection than for the one who shared it
        seeded? (and (seq stored) (empty? (:query-params request)))
        request (cond-> request seeded? (assoc :query-params stored))
        {:keys [params] :as req} (search-request! ctx request)]
    (assoc req
           :view      (request/view-param (:view params))
           :attr      (request/attr-param (:attr params))
           :at        (request/position-param (:at params))
           :docs      (some? (:docs params))
           :page      (request/page-param (:page params))
           :lang      (request/request-language request)
           :stored    stored
           :autosave? (settings/autosave? cookie)
           :seeded?   seeded?
           :transit?  (request/wants-transit? request))))

(defn runs?
  "True when the search `req` (see `read-request!`) runs: it carries a
  query. A form with none is a form, in either view, and is shown rather
  than answered."
  [{:keys [cqp]}]
  (boolean cqp))

(defn shown-params
  "The params the search page for `req` (see `read-request!`) shows in
  its form and cites in its links: what arrived, less the form's own
  query keys, with the query the form holds in the form's own spelling
  over it, so a control the mode does not read keeps what it carried;
  the corpora searched, or only those named when nothing runs, since a
  reader arriving at the form starts with none selected; and the
  grouping of the frequency view. The mode is the form's, which the
  radios read and no URL carries.

  A `:seeded?` form shows its settings as they are: how a query is
  matched rides on the query, and there is none, so re-spelling would
  drop what was stored."
  [{:keys [params arrived selected named seeded?] :as req}]
  (let [{:keys [form held]} arrived]
    (-> (if seeded?
          params
          (merge (apply dissoc params (mode/read-keys form params))
                 (query/->params form held)))
        (assoc :mode   (mode/form-of form)
               :corpus (if (runs? req) selected named)
               :attr   (request/attr-param (:attr params))
               :at     (request/position-param (:at params))))))

(defn citation
  "The URL params the search page for `req` (see `read-request!`) cites
  (see `shown-params` and dk.cst.corpus-probe.url/canonical), against its
  `:selectable` corpora."
  [{:keys [selectable] :as req}]
  (url/canonical (shown-params req) (set selectable)))

(defn cleared?
  "True when the submitted search `req` asks nothing: the reader
  emptying the field to start over (see
  dk.cst.corpus-probe.client.router/cleared?, the same rule with a
  script)."
  [{:keys [params]}]
  (nil? (query/of params)))

(defn uncited?
  "True when what `request` asks is not the citation `cited` of the
  search it describes: a form submits its defaults and its empty fields
  too, so a submitted form is never cited, while every link this app
  writes is. False for a `:seeded?` `req`, which was asked nothing."
  [request {:keys [seeded?] :as req} cited]
  (and (not seeded?)
       (not= (or (:query-string request) "") (url/query-string cited))))

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
  `corpora` via `ctx`: their union, positional attributes first, each
  kind keeping the registry order of the first corpus reporting it.
  Falls back to word, the one attribute every corpus has.

  Every attribute is offered whatever the query: a structural one cannot
  table a whole corpus, and that request is rejected with its reason, so
  the form still shows what was asked."
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
  corpora via `ctx`: the filters they offer, the `:selected` values of
  `params` and what its pattern and range fields hold."
  [ctx known params]
  (merge (frequency/filter-options! ctx known)
         {:selected (request/filter-params params)}
         (request/pattern-fields params)))

(defn value-lists!
  "The values of each positional attribute among `attrs` (keywords) that
  every one of `corpora` via `ctx` can list: attribute to its values over
  all of them, collated. An attribute one corpus cannot list, or lacks,
  has no entry, since a list missing part of what a reader may search for
  would mislead."
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
  "Prepare `result` for display: its counts (see `public-counts`), and
  its :pages once every corpus is counted."
  [result]
  (cond-> (public-counts result)
    (not (:remaining result)) (assoc :pages (url/page-count result))))

(defn search-outcome!
  "Search the `known` corpora for `cqp` via `ctx` with `opts` (see
  dk.cst.corpus-probe.search/concordance!): {:result <the concordance>},
  the `unknown` corpus names reported among its counts, or {:error ...}
  when no corpus was selected at all. Per-corpus errors travel inside
  the result."
  [ctx known unknown cqp opts]
  (if (and (empty? known) (empty? unknown))
    {:error {:type :no-corpus}}
    {:result (-> (search/concordance! ctx known cqp opts)
                 (update :counts into (unknown-counts unknown))
                 (public-result))}))

(defn linked-rows
  "The frequency `result` with a dk.cst.corpus-probe.url/subset-href on
  each of the rows the table shows, for the search described by `params`:
  the rows past those go unlinked, since the table does not show them and
  an export reads no links."
  [params {:keys [attr at] :as result}]
  (update result :rows
          (fn [rows]
            (into (mapv #(assoc % :href (url/subset-href params attr at
                                                         (:value %)))
                        (take frequency-views/row-limit rows))
                  (drop frequency-views/row-limit rows)))))

(defn frequency-outcome!
  "Table the `known` corpora for `cqp` (nil for the whole corpora) by
  `attr` via `ctx` with `opts` (see
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
                            :reach   (request/reach-param
                                      (:reach params))
                            :sample  (request/sample-param
                                      (:sample params))
                            ;; a document waits for the count; the client
                            ;; asks for it itself (see `serve-counts`)
                            :incremental? transit?))))

(defn form-data!
  "The state of the search form for `req` (see `read-request!`) via
  `ctx`, over the attribute descriptions `attrs` offered for its corpora
  (see `attr-options!`): the corpus chooser's `:folders`, the metadata
  `:filter-controls`, the `:search-attrs` a simple search may match and a
  concordance sorts by, the `:tokens` of the extended form, the
  `:value-lists` its fields suggest, the `:params` that fill the form
  (see `shown-params`), the same as `:asked`, and what a change of mode
  could not keep as `:switch`, for the form's status line."
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
  results region: its `:result` or `:error` from `outcome`, the language
  of each corpus searched as `:langs`, and the controls its view offers:
  the `:attrs` and `:positions` a frequency table groups by, from the
  attribute descriptions `attrs`, or the `:sort-modes` of a concordance
  and the `:export-limit` an export of it holds."
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
  cited as `cited` (see `citation`): to each view of the result as
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
  "The data dk.cst.corpus-probe.views/search-page renders one search page
  from, for `request` against `ctx`, or for the search `req` it reads
  with the citation `cited`: the state of the form (see `form-data!`),
  the outcome of the search when the params describe one, the links out
  of it, and the settings it holds as `:stored`, which the preferences
  box reads against the `:selectable` corpora both sides measure a
  selection by.

  The same map is embedded as transit for the client, so it holds corpus
  overviews only: the registry maps carry absolute server paths."
  ([ctx request]
   (let [req (read-request! ctx request)]
     (search-view-data ctx req (citation req))))
  ([ctx {:keys [lang view known stored selectable autosave?] :as req}
    cited]
   (let [outcome (run-view! ctx req)
         attrs   (attr-options! ctx known)]
     (merge {:lang       lang
             :view       view
             :cited      cited
             :autosave?  autosave?
             :selectable (set selectable)
             ;; encoded again rather than passed on as the cookie holds
             ;; it: the buttons compare this against the form, so both
             ;; come from the same writer. Expanded first, or a stored
             ;; selection of every corpus comes back out as an emptied one
             :stored     (settings/string
                          (url/with-every-corpus stored selectable)
                          selectable)}
            (form-data! ctx req attrs)
            (result-data req attrs outcome)
            (links req cited outcome)))))

(defn serve-search
  "Handle a search-page `request` against `ctx`: render the form, and
  when the query params describe a search, its concordance or the reason
  there is none, else the search help where the results will be.

  A request whose query string is not the search's citation (see
  `uncited?`) is answered with a redirect to it, so a submit without the
  client ends on the one URL the search has. Not one whose mode changed,
  whose citation is what the form holds rather than what it was given.

  The same request stores what it asked as the reader's own defaults,
  unless they turned that off. It is the reader without a script who is
  served here: a search they asked for says how they work, one they
  arrived at by a link says how the sharer does."
  [ctx request]
  (let [req     (read-request! ctx request)
        cited   (citation req)
        asked?  (uncited? request req cited)
        ;; the form as it stands, which is what its own buttons store
        ;; too (see dk.cst.corpus-probe.views.search/settings-now)
        cookies (when (and asked? (:autosave? req))
                  (request/preference-cookies
                   {settings/cookie-key (settings/string
                                         (shown-params req)
                                         (:selectable req))}))]
    (if (and (not (:transit? req))
             asked?
             (nil? (get-in req [:arrived :from])))
      {:status  303
       :headers (cond-> {"Location" (if (cleared? req)
                                      url/search
                                      (url/results-href cited))}
                  (seq cookies) (assoc "Set-Cookie" cookies))}
      (let [{:keys [result error] :as data} (search-view-data ctx req cited)
            ;; the masthead's navigation takes the citation; the client
            ;; does not read it
            data (cond-> (assoc (dissoc data :cited) :route :search)
                   (not (or result error))
                   (assoc :help (docs/document
                                 "help" (request/request-languages request))))]
        (cond-> (response/page-response request (views/title data) data cited)
          (seq cookies) (assoc-in [:headers "Set-Cookie"] cookies))))))

(defn serve-filters
  "Answer the metadata filters the corpora named in `request` offer via
  `ctx`, as transit, so the client can refresh the filter fieldset when
  the corpus selection changes without submitting a search.

  The values a reader has chosen are not answered: those are the reader's
  and the client is already holding them. An attribute list that no
  longer offers a chosen value leaves that value where it is, so
  narrowing the corpora never quietly drops part of a filter."
  [ctx request]
  (let [entries   (registry/entries ctx)
        named     (url/corpora-param (:corpus (:query-params request)))
        [known _] (corpus/split-known entries named)]
    (response/transit-response (frequency/filter-options! ctx known))))

(defn serve-counts
  "Answer the count of the search `request` describes against `ctx`, as
  transit: the per-corpus `:counts`, the `:size` and `:pages` of the
  result, the `:prev-href` and `:next-href` of the page named and the
  document `:title`, everything about a page the count decides. For the
  client, whose page arrived while its corpora were still being counted.

  Asked with the page's own params, so it counts the question the page
  answered; a request describing no search is refused."
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
                                    (select-keys opts [:filter :patterns
                                                       :ranges]))})]
        (response/transit-response
         (merge (select-keys result [:counts :size :pages])
                (url/page-hrefs (citation req) page result)
                {:title title}))))))
