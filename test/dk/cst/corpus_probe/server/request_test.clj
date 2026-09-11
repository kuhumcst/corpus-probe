(ns dk.cst.corpus-probe.server.request-test
  "What a request asks: the param readers and their defaults, the
  language negotiation and the preference cookies."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.server.request :as request]
            [dk.cst.corpus-probe.storage.settings :as settings]
            [dk.cst.corpus-probe.url :as url]))

(deftest position-param-test
  (is (= "match[-1]" (request/position-param "match[-1]")))
  (testing "anything but CQP's four positions is the start of the match"
    (is (= "match" (request/position-param nil)))
    (is (= "match" (request/position-param "target")))))

(deftest subset-param-test
  (is (= {:anchor "matchend[1]" :attr :lemma :value "kat"}
         (request/subset-param {:subset      "kat"
                                :subset-at   "matchend[1]"
                                :subset-attr "lemma"})))
  (testing "the anchor and attribute fall back as their params do"
    (is (= {:anchor "match" :attr :word :value "kat"}
           (request/subset-param {:subset "kat"}))))
  (testing "no value, no narrowing"
    (is (nil? (request/subset-param {:subset "" :subset-attr "lemma"})))
    (is (nil? (request/subset-param {})))))

(deftest context-param-test
  (is (= 10 (request/context-param "10")))
  (is (= :sentence (request/context-param "sentence")))
  (testing "anything else is the usual width"
    (is (= 5 (request/context-param nil)))
    (is (= 5 (request/context-param "0")))
    (is (= 5 (request/context-param "chapter")))))

(deftest near-param-test
  (testing "a word and how far away it may be"
    (is (= {:word "kat" :distance 3} (request/near-param " kat " "3"))))
  (testing "a distance that is not a positive integer is the default"
    (let [default (parse-long (:distance url/defaults))]
      (is (= {:word "kat" :distance default} (request/near-param "kat" nil)))
      (is (= {:word "kat" :distance default} (request/near-param "kat" "0")))
      (is (= {:word "kat" :distance default} (request/near-param "kat" "x")))))
  (testing "no word, nothing to be near"
    (is (nil? (request/near-param "" "5")))
    (is (nil? (request/near-param nil nil)))))

(deftest view-param-test
  (is (= :kwic (request/view-param nil)))
  (is (= :kwic (request/view-param "kwic")))
  (is (= :kwic (request/view-param "nonesuch")))
  (is (= :frequencies (request/view-param "frequencies"))))

(deftest accepted-languages-test
  (testing "every language offered, most preferred first, primary subtags"
    (is (= ["da" "en" "de"]
           (request/accepted-languages "de;q=0.5,da-DK,en;q=0.8"))))
  (testing "a language refused at quality 0 is left out, a blank offers none"
    (is (= ["da"] (request/accepted-languages "en;q=0,da")))
    (is (= [] (request/accepted-languages nil)))
    (is (= [] (request/accepted-languages "")))))

(deftest request-languages-test
  (let [languages (fn [headers] (request/request-languages {:headers headers}))]
    (testing "what the reader chose, then what they accept by quality, then
              the app's own Danish and English"
      (is (= ["en" "de" "da"] (languages {"cookie"          "lang=en"
                                          "accept-language" "de,da;q=0.8"})))
      (is (= ["de" "da" "en"] (languages {"accept-language" "de,da;q=0.8"})))
      (is (= ["de" "fr" "en" "da"]
             (languages {"accept-language" "de,en;q=0.7,fr;q=0.9"}))))
    (testing "a stored language we do not have is no choice at all"
      (is (= ["en" "da"] (languages {"cookie"          "lang=de"
                                     "accept-language" "en"}))))
    (testing "without either, the app's own"
      (is (= ["da" "en"] (languages {})))
      (is (= ["xx" "da" "en"] (languages {"accept-language" "xx"}))))
    (testing "the quality parameter is case-insensitive and may carry spaces"
      (is (= ["da" "en"] (languages {"accept-language" "da;Q=1"})))
      (is (= ["de" "en" "da"] (languages {"accept-language" "de , en ; q=0.9"}))))
    (testing "the URL has no say: a shared link imposes no language"
      (is (= ["da" "en"]
             (request/request-languages {:query-params {:lang "en"}}))))))

(deftest request-language-test
  (testing "the interface takes the first language it has"
    (is (= "en" (request/request-language
                 {:headers {"cookie" "lang=en" "accept-language" "da"}})))
    (is (= "en" (request/request-language
                 {:headers {"cookie" "lang=de" "accept-language" "en"}})))
    (is (= "en" (request/request-language
                 {:headers {"accept-language" "de,en;q=0.7,fr;q=0.9"}}))))
  (testing "without one it has, Danish"
    (is (= "da" (request/request-language {})))
    (is (= "da" (request/request-language
                 {:headers {"accept-language" "de"}})))
    (is (= "da" (request/request-language
                 {:headers {"accept-language" "en;q=0,da;q=0.1"}}))))
  (testing "the URL has no say: a shared link imposes no language"
    (is (= "da" (request/request-language {:query-params {:lang "en"}})))
    (is (= "en" (request/request-language
                 {:query-params {:lang "da"}
                  :headers      {"cookie" "lang=en"}})))))

(deftest valueless-param-test
  (testing "a query param written without a value names nothing"
    ;; Pedestal parses `?foo` into {nil "foo"}, and a nil key names no
    ;; param and no metadata filter
    (is (not (request/multi-param? nil)))
    (is (= "q=x" (url/query-string {nil "foo" :q "x"})))
    (is (= {} (request/filter-params {nil "foo"})))))

(deftest cookie-value-test
  (testing "a stored setting is read back by its name"
    (is (= "en" (request/cookie-value "lang=en" :lang)))
    (is (= "da" (request/cookie-value "other=1; lang=da; more=2" :lang))))
  (testing "a value the setting does not accept is not read back"
    (is (nil? (request/cookie-value "lang=xx" :lang)))
    (is (nil? (request/cookie-value "" :lang)))
    (is (nil? (request/cookie-value nil :lang)))))

(deftest preference-cookies-test
  (testing "a named setting with a value it accepts is stored for a year"
    (is (= ["lang=en;Path=/;Max-Age=31536000;SameSite=Lax"]
           (request/preference-cookies {:lang "en"}))))
  (testing "a value the setting refuses stores nothing, not a fallback"
    (is (= [] (request/preference-cookies {:lang "xx"}))))
  (testing "a name the allowlist does not carry cannot be stored at all"
    (is (= [] (request/preference-cookies {:session "stolen" :evil "x"})))
    (is (= ["lang=en;Path=/;Max-Age=31536000;SameSite=Lax"]
           (request/preference-cookies {:lang "en" :session "stolen"}))))
  (testing "so a caller can never choose both a cookie's name and its value"
    (is (every? #(str/starts-with? % "lang=")
                (request/preference-cookies {:lang    "da"
                                             :return  "/"
                                             "lang"   "xx"
                                             :Path    "/evil"})))))

(deftest stored-settings-test
  (let [stored (fn [cookie] (request/stored-settings {:headers {"cookie" cookie}}))]
    (testing "what a reader stored, as written, for its readers to take apart"
      (is (= "corpus=PROBE&view=frequencies"
             (stored "lang=en; settings=corpus=PROBE&view=frequencies"))))
    (testing "a reader who stored none has none"
      (is (nil? (stored "lang=en")))
      (is (nil? (request/stored-settings {}))))
    (testing "the settings are one setting, so forgetting them leaves the
              language alone"
      (is (= "" (stored "lang=en; settings=")))
      (is (= "en" (request/cookie-value "lang=en; settings=" :lang))))
    (testing "a value too long for a browser to keep is not read back"
      (is (nil? (stored (str "settings=corpus="
                             (apply str (repeat settings/max-length
                                                "X")))))))))

(deftest safe-return-test
  (is (= "/?q=x" (request/safe-return "/?q=x")))
  (is (= "/" (request/safe-return "//evil.example")))
  (is (= "/" (request/safe-return "https://evil.example")))
  (is (= "/" (request/safe-return nil))))

(deftest scalar-params-test
  (testing "a repeated scalar param keeps its first value, corpus its vector"
    (is (= {:q "a" :page "1" :corpus ["A" "B"]}
           (request/scalar-params {:q ["a" "b"] :page "1" :corpus ["A" "B"]}))))
  (testing "metadata filter params keep their vectors too"
    (is (= {:f.text_year ["1591" "1583"]}
           (request/scalar-params {:f.text_year ["1591" "1583"]})))))

(deftest filter-params-test
  (testing "f. params become the filter map, one value or several"
    (is (= {:text_year #{"1591" "1583"} :text_author #{"ukendt"}}
           (request/filter-params {:q             "hund"
                                   :f.text_year   ["1591" "1583"]
                                   :f.text_author "ukendt"}))))
  (testing "blank values are dropped, and with them empty attributes"
    (is (= {} (request/filter-params {:f.text_year ["" " "]}))))
  (testing "a param naming no attribute is dropped"
    (is (= {} (request/filter-params {:f. "x"}))))
  (is (= {} (request/filter-params {:q "hund" :corpus ["A"]}))))

(deftest pattern-params-test
  (testing "a pattern param is kept as the reader wrote it"
    (is (= {:text_title ["Hav.*"]}
           (request/pattern-params {:q "x" :fp.text_title "Hav.*"})))
    (testing "and a blank or nameless one is dropped"
      (is (= {} (request/pattern-params {:fp.text_title " " :fp. "x"})))))
  (testing "a range is the two numbers it names, for the corpus to answer
            from its own values rather than every number in between"
    (is (= {:text_year [1590 1592]}
           (request/range-params {:ff.text_year "1590" :ft.text_year "1592"})))
    (is (= {:text_year [1583 1583]}
           (request/range-params {:ff.text_year "1583" :ft.text_year "1583"})))
    (testing "and nothing at all when an end is missing, out of order or
              no whole number"
      (is (= {} (request/range-params {:ff.text_year "1590"})))
      (is (= {} (request/range-params {:ft.text_year "1590"})))
      (is (= {} (request/range-params {:ff.text_year "1592"
                                       :ft.text_year "1590"})))
      (is (= {} (request/range-params {:ff.text_year "1590"
                                       :ft.text_year "many"})))))
  (testing "the two are read apart, an attribute being able to carry both"
    (let [params {:fp.text_year "15.." :ff.text_year "1590"
                  :ft.text_year "1591"}]
      (is (= {:text_year ["15.."]} (request/pattern-params params)))
      (is (= {:text_year [1590 1591]} (request/range-params params))))))

(deftest pattern-fields-test
  (is (= {:patterns {:text_title "Hav.*"}
          :ranges   {:text_year ["1590" nil] :text_pages [nil "5"]}}
         (request/pattern-fields {:fp.text_title "Hav.*" :ff.text_year "1590"
                                  :ft.text_pages "5" :q "x"}))))

(deftest sample-param-test
  (is (= 100 (request/sample-param "100")))
  (testing "no sample is the whole result"
    (is (nil? (request/sample-param nil)))
    (is (nil? (request/sample-param ""))))
  (testing "a sample of none of the hits is no sample rather than an
            empty result, and neither is anything that is not a number"
    (is (nil? (request/sample-param "0")))
    (is (nil? (request/sample-param "-5")))
    (is (nil? (request/sample-param "many")))))

(deftest page-param-test
  (testing "the URL counts from one, the result from nought"
    (is (= 0 (request/page-param "1")))
    (is (= 2 (request/page-param "3"))))
  (testing "anything that is not a positive integer is the first page"
    (is (= 0 (request/page-param nil)))
    (is (= 0 (request/page-param "0")))
    (is (= 0 (request/page-param "-3")))
    (is (= 0 (request/page-param "x")))
    (is (= 0 (request/page-param "99999999999999999999")))))

(deftest attr-param-test
  (is (= "word" (request/attr-param nil)))
  (is (= "word" (request/attr-param "")))
  (is (= "lemma" (request/attr-param "lemma"))))

(deftest by-param-test
  (is (= :text_year (request/by-param "text_year")))
  (testing "no attribute, no second attribute"
    (is (nil? (request/by-param "")))
    (is (nil? (request/by-param nil)))))
