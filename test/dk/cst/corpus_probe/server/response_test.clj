(ns dk.cst.corpus-probe.server.response-test
  "The document shell and its protections: the mount points the client
  takes over, the renderer's quote bug undone, and the payload kept
  inside its script."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cognitect.transit :as transit]
            [dk.cst.corpus-probe.server.response :as response])
  (:import [java.io ByteArrayInputStream]))

(defn transit->
  "Decode transit-JSON string `s` (test helper, mirroring
  response/->transit)."
  [s]
  (let [in (ByteArrayInputStream. (.getBytes s "UTF-8"))]
    (transit/read (transit/reader in :json))))

(deftest document-test
  (let [switch {"da" "/?lang=da" "en" "/?lang=en"}
        base   {:lang "en" :switch switch :title "T"
                :body [:main {:id "main"} "body"]}
        plain  (response/document base)
        client (response/document (assoc base :payload "[]"))
        ;; nil rather than -1 for absent, so an order assertion cannot
        ;; pass on a part that is not there at all
        at     (fn [doc s] (let [i (.indexOf ^String doc ^String s)]
                             (when-not (neg? i) i)))]
    (testing "the bypass link is the document's first focusable element"
      (is (some? (at plain "href=\"#main\"")))
      (is (< (at plain "href=\"#main\"") (at plain "<header"))))
    (testing "the footer follows the body, so it is the document's own"
      (is (some? (at plain "<footer")))
      (is (< (at plain "<main") (at plain "<footer"))))
    (testing "every page mounts the client, so every page can route"
      (is (str/includes? plain "<div id=\"app\">"))
      (is (str/includes? plain "/js/main.js"))
      (is (str/includes? client "<div id=\"app\">")))
    (testing "the masthead and the footer mount too: a routed language
              switch re-words both without a reload"
      (is (str/includes? plain "<div id=\"masthead\"><header"))
      (is (str/includes? plain "<div id=\"footer\"><footer")))
    (testing "only a page given one carries a bootstrap payload"
      (is (not (str/includes? plain "id=\"bootstrap\"")))
      (is (str/includes? client "id=\"bootstrap\"")))
    (testing "a page with no payload still gets its <main>"
      (is (str/includes? plain "<main")))))

(deftest shell-data-test
  (let [request {:uri "/search" :query-params {:q "hund" :corpus "PROBE"}}
        data    (response/shell-data request {:q "hund" :corpus ["PROBE"]})]
    (testing "the masthead travels in the view data, so a routed navigation
              re-renders it rather than leaving last render's links"
      (is (= "/search" (:path data)))
      (is (contains? data :nav)))
    (testing "returning to the search keeps the query"
      (is (= "/search?q=hund&corpus=PROBE#results" (:search (:nav data)))))
    (testing "and without one is the bare search page"
      (is (= "/search" (:search (:nav (response/shell-data request {}))))))
    (testing "no URL names a language: that is the reader's own preference"
      (is (= "/corpora" (:corpora-heading (:nav data))))
      (is (= "/glossary" (:glossary (:nav data))))
      (is (not (str/includes? (:search (:nav data)) "lang="))))
    (testing "the frequency table is not a place: it is a view of a result"
      (is (not (contains? (:nav data) :frequencies))))))

(deftest correct-quote-escaping-test
  (testing "corrupted double quotes (&#39;) are restored to &#34;"
    (is (= "[lemma=&#34;hund&#34;]"
           (response/correct-quote-escaping "[lemma=&#39;hund&#39;]"))))
  (testing "real apostrophes (&apos;) are left untouched"
    (is (= "it&apos;s" (response/correct-quote-escaping "it&apos;s")))))

(deftest script-safe-test
  (testing "< is neutralised so corpus content cannot terminate the script"
    (is (= "a\\u003c/script>b" (response/script-safe "a</script>b"))))
  (testing "a hostile value survives embedding and decoding"
    (let [data    {:hits [{:word "12\"" :tag "</script>"}]}
          payload (response/script-safe (response/->transit data))]
      (is (not (re-find #"</script" payload)))
      ;; the browser reads the script text verbatim; the JSON reader decodes
      ;; the < escapes, which we emulate here before decoding.
      (is (= data (transit-> (str/replace payload "\\u003c" "<")))))))

(deftest transit-response-test
  (testing "the data behind a route varies by what the document varies by,
            so a shared cache cannot hand one reader's page to the next"
    (let [{:keys [status headers body]} (response/transit-response {:a 1})]
      (is (= 200 status))
      (is (str/starts-with? (get headers "Content-Type")
                            "application/transit+json"))
      (is (= "Accept, Accept-Language, Cookie" (get headers "Vary")))
      (is (= {:a 1} (transit-> body))))))
