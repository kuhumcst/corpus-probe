(ns dk.cst.corpus-probe.storage-test
  "How a thing is kept in a reader's browser: the cookie the server
  writes and the client writes back."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.storage :as storage]))

(deftest cookie-test
  (testing "a setting outlives the visit that set it, site-wide and on
            same-site requests only"
    (is (= (str "lang=en;Path=/;Max-Age=" storage/cookie-max-age
                ";SameSite=Lax")
           (storage/cookie :lang "en"))))
  (testing "a setting stored as nothing is a setting forgotten"
    (is (= "settings=;Path=/;Max-Age=0;SameSite=Lax"
           (storage/cookie :settings "")))))
