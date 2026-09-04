(ns dk.cst.corpus-probe.i18n
  "The Danish and English user interface: the dictionary of interface
  strings and the lookup the views render them through.

  The chosen language travels in the application state as :lang, so the
  client re-renders the shared .cljc views in the same language the server
  first painted them in. A view that already takes the application state
  or an options map reads :lang from it; every other view takes the
  language code as its first argument.

  Only the interface is translated. CQP's own error messages, attribute
  names, corpus titles and corpus content are shown verbatim, in their own
  language. So are the TSV and CSV exports, whose column names are CWB's
  own and are read by other programs."
  (:require [clojure.string :as str]))

(def languages
  "The supported UI language codes, in display order."
  ["da" "en"])

(def default-language
  "The language served when the request asks for none we have: Danish,
  the language of the readers this replaces KORP for."
  "da")

(defn supported?
  "True when `lang` is one of the `languages`."
  [lang]
  (boolean (some #{lang} languages)))

(def dictionary
  "Every user interface string, keyed by what it names, in each of the
  `languages`. A word that differs by role (the noun heading the search
  page, the verb on its button) is a key of its own."
  {;; the bypass link, the site header, its navigation, the footer and the
   ;; document metadata
   :skip-to-content {:da "Gå til indhold" :en "Skip to content"}
   :powered-by      {:da "Drevet af" :en "Powered by"}
   :subtitle        {:da "CWB-korpussøgning" :en "CWB corpus search"}
   :site            {:da "Websted" :en "Site"}
   :language        {:da "Sprog" :en "Language"}
   :search          {:da "Søgning" :en "Search"}
   :frequencies     {:da "Frekvenser" :en "Frequencies"}
   ;; a -heading key is the capitalised form of the key beside it, as
   ;; :corpus and :corpus-heading are
   :corpora         {:da "korpusser" :en "corpora"}
   :corpora-heading {:da "Korpusser" :en "Corpora"}
   :description     {:da "Søg i CWB-korpusser og læs KWIC-konkordanser."
                     :en "Search CWB corpora and read KWIC concordances."}

   ;; the search form
   :metadata        {:da "Metadata" :en "Metadata"}
   :query           {:da "Forespørgsel" :en "Query"}
   :query-example   {:da "[lemma = \"hund\"] eller almindelige ord"
                     :en "[lemma = \"hund\"] or plain words"}
   :query-mode      {:da "Forespørgselstype" :en "Query mode"}
   :simple          {:da "Simpel" :en "Simple"}
   :simple-options  {:da "Indstillinger for simpel søgning"
                     :en "Simple-search options"}
   :ignore-case     {:da "ignorer store og små bogstaver" :en "ignore case"}
   :starts-with     {:da "starter med" :en "starts with"}
   :ends-with       {:da "slutter med" :en "ends with"}
   :submit          {:da "Søg" :en "Search"}
   :sort            {:da "Sortering" :en "Sort"}
   :group-by        {:da "Gruppér efter" :en "Group by"}
   :selected        {:da "valgt" :en "selected"}
   :too-many-values {:da "For mange værdier til at vise: "
                     :en "Too many values to list: "}
   :region          {:da "region" :en "region"}
   :regions         {:da "regioner" :en "regions"}

   ;; the sort modes (see dk.cst.corpus-probe.query/sort-modes)
   :sort-corpus     {:da "korpusrækkefølge" :en "corpus order"}
   :sort-word       {:da "match" :en "match"}
   :sort-left       {:da "venstre kontekst" :en "left context"}
   :sort-right      {:da "højre kontekst" :en "right context"}
   :sort-random     {:da "tilfældig" :en "random"}

   ;; the concordance and its summary
   :concordance     {:da "Konkordans" :en "Concordance"}
   :no-hits         {:da "Ingen træf." :en "No hits."}
   :hit             {:da "træf" :en "hit"}
   :hits            {:da "træf" :en "hits"}
   :in              {:da "i" :en "in"}
   :within          {:da "inden for" :en "within"}
   :page            {:da "side" :en "page"}
   :of              {:da "af" :en "of"}
   :hits-per-corpus {:da "Træf pr. korpus" :en "Hits per corpus"}
   :corpus          {:da "korpus" :en "corpus"}
   :error           {:da "fejl" :en "error"}
   :pagination      {:da "Sidenavigation" :en "Pagination"}
   :previous        {:da "forrige" :en "previous"}
   :next            {:da "næste" :en "next"}
   :wider-context   {:da "Vis eller skjul bredere kontekst"
                     :en "Toggle wider context"}
   :these-freqs     {:da "Frekvenser for disse træf"
                     :en "Frequencies of these hits"}
   :this-concordance {:da "Konkordans for disse træf"
                      :en "Concordance of these hits"}
   :download        {:da "Download" :en "Download"}
   :the-first       {:da "de første" :en "the first"}
   :all-values      {:da "alle værdier" :en "all values"}

   ;; the token inspector
   :token-details   {:da "Tokendetaljer" :en "Token details"}
   :close           {:da "Luk" :en "Close"}
   :token           {:da "Token" :en "Token"}
   :text            {:da "Tekst" :en "Text"}
   :corpus-heading  {:da "Korpus" :en "Corpus"}

   ;; the frequency table
   :all-tokens      {:da "Alle tokens" :en "All tokens"}
   :by              {:da "efter" :en "by"}
   :value           {:da "værdi" :en "value"}
   :values          {:da "værdier" :en "values"}
   ;; :the and :most-frequent wrap a count: "the 500 most frequent shown"
   :the             {:da "de" :en "the"}
   :most-frequent   {:da "hyppigste vises" :en "most frequent shown"}
   :frequency       {:da "frekvens" :en "frequency"}
   :per-million     {:da "pr. million" :en "per million"}
   :total           {:da "i alt" :en "total"}
   :p-attrs         {:da "positionelle attributter"
                     :en "positional attributes"}
   :s-attrs         {:da "strukturelle attributter"
                     :en "structural attributes"}

   ;; the corpus index and info pages
   :other           {:da "Andre" :en "Other"}
   :tokens          {:da "tokens" :en "tokens"}
   :no-data         {:da "ingen data" :en "no data"}
   :unavailable     {:da "utilgængelig" :en "unavailable"}
   :size            {:da "størrelse" :en "size"}
   :charset         {:da "tegnsæt" :en "charset"}
   :p-attrs-heading {:da "Positionelle attributter"
                     :en "Positional attributes"}
   :s-attrs-heading {:da "Strukturelle attributter"
                     :en "Structural attributes"}
   :a-attrs-heading {:da "Alignment-attributter"
                     :en "Alignment attributes"}
   :attribute       {:da "attribut" :en "attribute"}
   :types           {:da "typer" :en "types"}
   :annotations     {:da "annotationer" :en "annotations"}
   :with-annots     {:da "med annotationer" :en "with annotations"}
   :blocks          {:da "blokke" :en "blocks"}
   :info            {:da "Info" :en "Info"}
   :unreadable      {:da "Korpusset kunne ikke læses"
                     :en "Could not read corpus"}
   :unreadable-why  {:da "CWB kunne ikke læse dette korpus' datafiler."
                     :en "CWB could not read this corpus's data files."}
   :undefined-why   {:da (str "Registret har dette korpus, men CWB har "
                              "ingen data til det.")
                     :en (str "The registry lists this corpus, but CWB has "
                              "no data for it.")}
   :search-in       {:da "Søg i" :en "Search"}
   :word-freqs      {:da "Ordfrekvenser i" :en "Word frequencies of"}

   ;; the error headings (see dk.cst.corpus-probe.views.page/error-types)
   :cqp-error       {:da "CQP-fejl" :en "CQP error"}
   :timeout         {:da "Forespørgslen tog for lang tid"
                     :en "The query timed out"}
   :no-corpus       {:da "Intet korpus valgt" :en "No corpus selected"}
   :unknown-corpus  {:da "Ukendt korpus" :en "Unknown corpus"}
   :rejected        {:da "Forespørgslen blev afvist" :en "Request rejected"}
   :misaligned      {:da "CQP's output kunne ikke læses"
                     :en "CQP output could not be read"}
   :internal        {:da "Uventet fejl" :en "Unexpected error"}
   :no-corpus-why   {:da "Vælg mindst ét korpus at søge i."
                     :en "Select at least one corpus to search."}
   :unknown-corpus-why {:da "Registret har intet korpus med det navn."
                        :en "The registry has no corpus by that name."}
   :misaligned-why  {:da "CQP udskrev noget andet end de ønskede rækker."
                     :en (str "CQP printed something other than the "
                              "requested rows.")}
   :internal-why    {:da (str "Søgningen mislykkedes på serveren; "
                              "detaljerne står i serverens log.")
                     :en (str "The search failed on the server; its log has "
                              "the details.")}

   ;; the two separators of a number, which Danish and English swap
   :digit-separator {:da "." :en ","}
   :decimal-separator {:da "," :en "."}})

(defn tr
  "The dictionary string named by `k` in language `lang`.

  Falls back to the name of `k`, so a key no translation covers shows up
  in the page as itself rather than as nothing."
  [lang k]
  (get-in dictionary [k (keyword lang)] (name k)))

(defn group-digits
  "Write number `n` the way `lang` writes it: its digits grouped in
  thousands, its fraction (when it has one) after the decimal separator;
  nil for nil, so a statistic that could not be computed shows as nothing.

  Danish and English swap the two separators, so a rate beside a grouped
  count is ambiguous unless both follow the same language.

  (group-digits \"da\" 64600000)
  ;; => \"64.600.000\"

  (group-digits \"da\" 1234.5)
  ;; => \"1.234,5\""
  [lang n]
  (when (some? n)
    (let [[whole fraction] (str/split (str n) #"\.")]
      (cond-> (->> (reverse whole)
                   (partition-all 3)
                   (map (comp str/join reverse))
                   (reverse)
                   (str/join (tr lang :digit-separator)))
        fraction (str (tr lang :decimal-separator) fraction)))))

(comment
  (tr "da" :submit)
  ;; => "Søg"

  ;; a key the dictionary does not cover renders as itself
  (tr "en" :nonesuch)
  ;; => "nonesuch"
  #_.)
