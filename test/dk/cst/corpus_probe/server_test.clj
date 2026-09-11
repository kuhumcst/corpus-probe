(ns dk.cst.corpus-probe.server-test
  "How the server is configured: what an installation can change without
  rebuilding, and what it cannot; the route table, and the handlers the
  server keeps for itself: the documents and the preferences."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.server :as server]
            [dk.cst.corpus-probe.url :as url]))

(defn with-config-file
  "Call no-arg `f` with `content` written to a temp file and named by the
  config property, and put the property back afterwards."
  [content f]
  (let [file (fs/file (fs/create-temp-dir) "config.edn")
        was  (System/getProperty server/config-property)]
    (spit file content)
    (try
      (System/setProperty server/config-property (str file))
      (f (str file))
      (finally
        (if was
          (System/setProperty server/config-property was)
          (System/clearProperty server/config-property))))))

(deftest content-security-policy-test
  (testing "what ships is strict: no eval, and no origin but this one"
    (let [policy (server/content-security-policy {})]
      (is (= (str "default-src 'self'; script-src 'self'; "
                  "style-src 'self'; img-src 'self' data:")
             policy))
      (is (not (re-find #"unsafe-eval" policy)))
      (is (not (re-find #"connect-src" policy)))))
  (testing "a watch is let through only where one is configured"
    (let [policy (server/content-security-policy
                  {:dev-client "ws://localhost:9630"})]
      (is (re-find #"script-src 'self' 'unsafe-eval'" policy))
      (is (re-find #"connect-src 'self' ws://localhost:9630" policy))))
  (testing "the app's own fetches survive the widening"
    (is (re-find #"connect-src 'self'"
                 (server/content-security-policy {:dev-client "ws://x"})))))

(deftest read-config-test
  (testing "with nothing named, the built-in configuration is what runs"
    (let [config (server/read-config)]
      (is (nil? (:config-file config)))
      (is (= 300000 (:query-timeout-ms config)))
      (is (seq (:folders config)))))
  (testing "paths are absolute, since cqp does not share our directory"
    (is (fs/absolute? (:registry (server/read-config))))
    (is (fs/absolute? (:cache-dir (server/read-config))))))

(deftest config-file-test
  (with-config-file
    (pr-str {:query-timeout-ms 60000
             :cache-max-bytes  42
             :registry         "/srv/corpora/registry"})
    (fn [path]
      (let [config (server/read-config)]
        (testing "an installation's file wins over the built-in one"
          (is (= 60000 (:query-timeout-ms config)))
          (is (= 42 (:cache-max-bytes config)))
          (is (= "/srv/corpora/registry" (:registry config))))
        (testing "and what it leaves out is left as it was"
          (is (seq (:folders config)))
          (is (= 60000 (:timeout-ms config))))
        (testing "the file it read is part of what it read, so the log says
                  which one won"
          (is (= path (:config-file config))))))))

(deftest missing-config-file-test
  (let [was (System/getProperty server/config-property)]
    (try
      (System/setProperty server/config-property "/no/such/config.edn")
      (testing "a file that is named but not there stops the server"
        (is (thrown? Exception (server/read-config))))
      (finally
        (if was
          (System/setProperty server/config-property was)
          (System/clearProperty server/config-property))))))

(deftest malformed-config-file-test
  (with-config-file "{:port 7373"
    (fn [_]
      (testing "so does one that is not readable EDN"
        (is (thrown? Exception (server/read-config)))))))

(deftest routes-test
  (let [routes (server/routes {:registry "test/resources"})
        paths  (set (map first routes))]
    (testing "every path the URLs name is served, and the assets beside them"
      (is (every? paths [url/home url/search url/corpora url/glossary
                         url/cqp-guide url/preferences
                         url/filters-api url/counts-api
                         "/css/*path" "/js/*path" "/fonts/*path"])))
    (testing "each route names itself, once"
      (is (= (count routes)
             (count (distinct (map #(nth % 4) routes))))))))

(deftest serve-document-test
  (let [page (fn [name lang]
               (server/serve-document {} name
                                      {:headers {"cookie" (str "lang=" lang)}}))]
    (testing "a document names its page by its own heading, in the language read"
      (is (= "Corpus search · corpus-probe"
             (second (re-find #"<title>([^<]*)" (:body (page "frontpage" "en"))))))
      (is (= "Ordliste · corpus-probe"
             (second (re-find #"<title>([^<]*)" (:body (page "glossary" "da")))))))
    (testing "and is a document page, headed by the document"
      (let [body (:body (page "glossary" "en"))]
        (is (str/includes? body "<main id=\"main\" tabindex=\"-1\" class=\"document\">"))
        (is (str/includes? body "<h1 id=\"glossary\">Glossary</h1>"))
        (is (str/includes? body "<dt id=\"kwic\">KWIC</dt>"))))))

(deftest serve-preferences-test
  (let [post (fn [params] (server/serve-preferences nil {:form-params params}))]
    (testing "the choice is stored and the reader sent back where they were"
      (let [{:keys [status headers]} (post {:lang "en" :return "/?q=hund"})]
        (is (= 303 status))
        (is (= "/?q=hund" (get headers "Location")))
        (is (= ["lang=en;Path=/;Max-Age=31536000;SameSite=Lax"]
               (get headers "Set-Cookie")))))
    (testing "a return that names anywhere but this app is not followed"
      (is (= "/" (get-in (post {:lang "en" :return "https://evil.example/"})
                         [:headers "Location"])))
      (is (= "/" (get-in (post {:lang "en" :return "//evil.example/"})
                         [:headers "Location"]))))
    (testing "a request that stores nothing sets no cookie header at all"
      (is (not (contains? (:headers (post {:lang "xx" :return "/"}))
                          "Set-Cookie"))))))

(deftest serve-file-test
  (let [file (fn [path]
               (server/serve-file "text/css; charset=utf-8" "css" nil
                                  {:path-params {:path path}}))]
    (testing "a stylesheet is served as such, and is not kept"
      (is (= 200 (:status (file "style.css"))))
      (is (= "text/css; charset=utf-8"
             (get-in (file "style.css") [:headers "Content-Type"])))
      (is (= "no-store" (get-in (file "style.css") [:headers "Cache-Control"]))))
    (testing "a path out of the directory, or to nothing, is not found"
      (is (= 404 (:status (file "../config.edn"))))
      (is (= 404 (:status (file "nonesuch.css"))))))
  (testing "the font is served as the bytes it is, and kept, since a face
            that is refetched is a face `font-display: optional` never
            reaches a second load to apply on"
    (let [{:keys [status headers body]}
          (server/serve-file "font/woff2" "fonts" server/font-cache
                             {:path-params {:path "figtree-latin.woff2"}})
          head (with-open [in body] (.readNBytes in 4))]
      (is (= 200 status))
      (is (= "font/woff2" (get headers "Content-Type")))
      (is (= (str "public, max-age=" server/font-cache)
             (get headers "Cache-Control")))
      ;; the signature of a woff2, which a body read as text would lose
      (is (= "wOF2" (String. head "ISO-8859-1"))))))
