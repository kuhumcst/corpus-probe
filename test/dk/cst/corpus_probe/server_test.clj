(ns dk.cst.corpus-probe.server-test
  "How the server is configured: what an installation can change without
  rebuilding, and what it cannot."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.server :as server]))

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
