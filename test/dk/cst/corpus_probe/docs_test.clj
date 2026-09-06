(ns dk.cst.corpus-probe.docs-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.docs :as docs]
            [dk.cst.corpus-probe.i18n :as i18n]
            [dk.cst.corpus-probe.url :as url]
            [dk.cst.corpus-probe.views.hiccup :refer [deep]]))

(deftest ->hiccup-test
  (testing "the blocks of the document, headings carrying an id"
    (is (= [[:h1 {:id "a-title"} "A title"]
            [:p "some " [:em "prose"] " and " [:code "code"]]]
           (docs/->hiccup "# A title\n\nsome *prose* and `code`"))))
  (testing "a heading may name its own id, which then holds in every language"
    (is (= [[:h2 {:id "kwic"} "Konkordans"]]
           (docs/->hiccup "## Konkordans {#kwic}")))
    (is (= [[:h2 {:id "cpos"} [:code "cpos"] ""]]
           (docs/->hiccup "## `cpos` {#cpos}")))
    (testing "and so may the term of a definition list"
      (is (= [[:dl [:dt {:id "kwic"} "KWIC"] [:dd "key word in context"]]]
             (docs/->hiccup "KWIC {#kwic}:\n  key word in context")))))
  (testing "raw HTML renders as nothing, not as markup or an error"
    (let [html (docs/->hiccup "# A\n\n<!-- note -->\n\n<div>x</div>\n\nb <b>c</b>")]
      (is (= [[:h1 {:id "a"} "A"] [:p "b " "c"]] html))
      (is (not (some #{:div :b} (deep html)))))))

(deftest hiccup-test
  (testing "a document in the reader's language"
    (is (= [:h1 {:id "cqp-vejledning"} "CQP-vejledning"]
           (first (docs/hiccup "cqp-guide" ["da"]))))
    (is (= [:h1 {:id "cqp-guide"} "CQP guide"]
           (first (docs/hiccup "cqp-guide" ["en"])))))
  (testing "a language without a file falls through to the next read"
    (is (= (docs/hiccup "help" ["da"]) (docs/hiccup "help" ["xx" "da" "en"])))
    (is (= (docs/hiccup "help" ["en"]) (docs/hiccup "help" ["xx" "en"]))))
  (testing "no file in any language, nothing"
    (is (nil? (docs/hiccup "help" ["xx"])))
    (is (nil? (docs/hiccup "nonesuch" ["en"])))))

(deftest title-test
  (is (= "Query help" (docs/title [[:p "x"] [:h1 {:id "a"} "Query help"]])))
  (is (= "The cpos column"
         (docs/title [[:h2 {:id "a"} "The " [:code "cpos"] " column"]])))
  (is (nil? (docs/title [[:p "x"]]))))


(def documents
  "The name of every document the app serves."
  ["frontpage" "help" "cqp-guide" "glossary"])

(deftest documents-test
  (testing "every document exists in every language the interface has"
    (doseq [name documents
            lang i18n/languages]
      (is (some? (docs/resource name [lang])) (str name " in " lang))
      (is (str/ends-with? (str (docs/resource name [lang]))
                          (str name "." lang ".md"))
          (str name " in " lang " is its own file"))))
  (testing "every link the frontpage makes into the app is a page it has"
    (doseq [lang i18n/languages]
      (let [hrefs (->> (deep (docs/hiccup "frontpage" [lang]))
                       (filter #(and (map? %) (:href %)))
                       (map :href)
                       (filter #(str/starts-with? % "/")))]
        (is (seq hrefs))
        (is (every? #(or (#{url/search url/corpora url/glossary
                            url/cqp-guide} %)
                         (str/starts-with? % (str url/glossary "#")))
                    hrefs)))))
  (testing "the help has no heading: it is a key to the form, not a page"
    (doseq [lang i18n/languages]
      (is (not-any? docs/heading? (docs/hiccup "help" [lang])) lang)))
  (testing "the CQP guide's examples are CQP's own, untranslated"
    (doseq [lang i18n/languages]
      (is (some #{[:code "[lemma = \"x\"]"]}
                (deep (docs/hiccup "cqp-guide" [lang])))))))

(defn glossary-terms
  "The ids of the terms of the glossary in `lang`, in order."
  [lang]
  (->> (deep (docs/hiccup "glossary" [lang]))
       (filter #(and (vector? %) (= :dt (first %))))
       (map (comp :id second))))

(deftest glossary-links-test
  (testing "the glossary is one definition list, and every term the
            interface links has an entry in every language"
    (doseq [lang i18n/languages]
      (is (= 1 (count (filter #(= :dl (first %))
                              (docs/hiccup "glossary" [lang])))))
      (doseq [id ["kwic" "concordance" "cqp" "cpos" "match" "frequency"
                  "hit" "metadata" "positional-attributes"
                  "structural-attributes" "alignment-attributes"
                  "per-million" "region"]]
        (is (some #{id} (glossary-terms lang)) (str id " in " lang)))))
  (testing "every glossary link in every document names a term the glossary
            has, in that language"
    (doseq [lang i18n/languages]
      (let [terms (set (glossary-terms lang))]
        (doseq [name documents
                href (->> (deep (docs/hiccup name [lang]))
                          (filter #(and (map? %) (:href %)))
                          (map :href)
                          (filter #(str/starts-with? % (str url/glossary "#"))))]
          (is (contains? terms (subs href (inc (count url/glossary))))
              (str href " from " name " in " lang))))))
  (testing "every language deep-links the same sections of the CQP manual"
    (let [manual-url "https://cwb.sourceforge.io/files/CQP_Manual/"
          manual     (fn [lang]
                       (->> (deep (docs/hiccup "glossary" [lang]))
                            (filter #(and (map? %) (:href %)))
                            (map :href)
                            (filter #(str/starts-with? % manual-url))
                            (sort)))]
      (is (< 10 (count (manual "en"))))
      (is (apply = (map manual i18n/languages))))))

(deftest glossary-order-test
  (testing "the glossary is in alphabetical order, as its language sorts"
    (doseq [lang i18n/languages]
      (let [collator (java.text.Collator/getInstance
                      (java.util.Locale/forLanguageTag lang))
            terms    (->> (deep (docs/hiccup "glossary" [lang]))
                          (filter #(and (vector? %) (= :dt (first %))))
                          (map #(apply str (filter string?
                                                   (tree-seq coll? seq
                                                             (drop 2 %))))))]
        (is (= terms (sort-by identity collator terms)) lang)))))
