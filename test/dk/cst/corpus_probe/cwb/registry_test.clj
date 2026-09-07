(ns dk.cst.corpus-probe.cwb.registry-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.cwb.registry :as registry]
            [dk.cst.corpus-probe.test.cwb
             :refer [encode! fixture mismatched-entry temp-registry!]]))

(deftest entry-test
  (let [entry (registry/entry "test/resources/registry-probe")]
    (is (= "probe" (:id entry)))
    (is (= "" (:name entry)))
    (is (= "/corpora/data/probe" (:home entry)))
    (is (= "utf8" (:charset entry)))
    (is (= "??" (:language entry)))
    (testing "p-attributes keep declaration (= display) order"
      (is (= [:word :pos :lemma] (:p-attrs entry))))
    (testing "s-attributes include the split-off annotation attributes"
      (is (some #{:text_title} (:s-attrs entry))))
    (is (= [] (:aligned entry)))))

(deftest entries-test
  (let [reg (temp-registry! fixture {"probe2" (mismatched-entry fixture)
                                    "readme" "not a registry entry\n"})]
    (fs/create-dir (fs/file reg "old"))
    (let [entries (registry/entries {:registry reg})]
      (testing "the ID is the filename, which is the name CQP resolves"
        (is (= ["probe" "probe2"] (map :id entries))))
      (testing "stray files and subdirectories are not corpora"
        (is (= 2 (count entries)))))))

(deftest entry-of-test
  (let [ctx {:registry "test/resources"}]
    (testing "an entry is found by its name in either case, its id the
              filename"
      (is (= "registry-probe" (:id (registry/entry-of ctx "Registry-Probe"))))
      (is (= [:word :pos :lemma]
             (:p-attrs (registry/entry-of ctx "REGISTRY-PROBE")))))
    (testing "a name that is no corpus name never becomes a path"
      (is (nil? (registry/entry-of ctx "bad; exit")))
      (is (nil? (registry/entry-of ctx "../resources/registry-probe"))))
    (testing "a name with no entry, or a directory, is nothing"
      (is (nil? (registry/entry-of ctx "nope")))
      (is (nil? (registry/entry-of ctx "golden"))))))

(deftest language-test
  (is (= "da" (registry/language {:language "da"})))
  (testing "the ?? placeholder and other junk are not a language"
    (is (nil? (registry/language {:language "??"})))
    (is (nil? (registry/language {})))))

(deftest charset-test
  (testing "the registry's charset property maps to a Java charset name"
    ;; entry-file lowercases the corpus name, finding the test fixture
    (is (= "UTF-8" (registry/charset {:registry "test/resources"}
                                     "REGISTRY-PROBE"))))
  (testing "missing registry entries fall back to UTF-8"
    (is (= "UTF-8" (registry/charset {:registry "test/resources"} "NOSUCH"))))
  (is (= "ISO-8859-1" (registry/cwb->charset "latin1"))))

(deftest charset-of-directory-test
  (testing "a subdirectory named like a corpus is not read as an entry"
    (let [reg (temp-registry! fixture {})]
      (fs/create-dir (fs/file reg "old"))
      (is (= "UTF-8" (registry/charset {:registry reg} "OLD"))))))

(deftest build-stamp-test
  (let [ctx    (encode! "aaa" "utf8")
        before (registry/build-stamp ctx "PROBE")]
    (testing "the stamp follows the corpus data, not the registry entry"
      ;; cwb-encode rewrites the entry only when passed -R, so encoding
      ;; a corpus in place leaves it byte-identical while the data change
      (spit (fs/file (:home ctx) "word.corpus") "bbbb")
      (is (not= before (registry/build-stamp ctx "PROBE"))))
    (testing "a charset correction changes which matches exist, so it
              must change the stamp, though it touches no data"
      (let [before (registry/build-stamp ctx "PROBE")]
        (spit (fs/file (:registry ctx) "probe")
              (str "NAME \"\"\nID probe\nHOME " (:home ctx)
                   "\nATTRIBUTE word\n##:: charset = \"latin1\"\n"))
        (is (not= before (registry/build-stamp ctx "PROBE")))))
    (testing "an entry whose data cannot be found still stamps"
      (is (some? (registry/build-stamp {:registry "test/resources"}
                                       "REGISTRY-PROBE"))))))

(deftest folder-corpora-test
  (is (= [:a :b :c]
         (registry/folder-corpora {:corpora [:a]
                                   :folders [{:corpora [:b]
                                              :folders [{:corpora [:c]}]}]}))))

(deftest grouped-corpora-test
  (let [probe     {:id "PROBE" :title nil :size 47}
        viser     {:id "VISER" :title "Folkeviser" :size 48}
        taler     {:id "TALER" :title "Taler" :size 42}
        overviews [probe taler viser]
        folders   [{:label   "Litteratur"
                    :folders [{:label "Folkeviser" :corpora ["VISER"]}]}
                   {:label   "Folketinget"
                    :corpora ["TALER" "GONE"]}]
        grouped   (registry/grouped-corpora folders overviews)]
    (testing "nested folders resolve their corpus IDs to overviews"
      (is (= [viser] (-> grouped first :folders first :corpora))))
    (testing "IDs the registry does not know are dropped"
      (is (= [taler] (:corpora (second grouped)))))
    (testing "unclaimed corpora follow as a label-less folder"
      (is (= {:label nil :corpora [probe] :folders []} (last grouped))))
    (testing "no configuration means one label-less folder of everything"
      (is (= [{:label nil :corpora overviews :folders []}]
             (registry/grouped-corpora nil overviews))))
    (testing "everything claimed means no trailing folder"
      (is (= 1 (count (registry/grouped-corpora [{:label   "All"
                                                  :corpora ["PROBE" "TALER"
                                                            "VISER"]}]
                                                overviews)))))
    (testing "folders the registry leaves empty are dropped, at any depth"
      (is (= [{:label "Litteratur" :corpora []
               :folders [{:label "Folkeviser" :corpora [viser] :folders []}]}
              {:label nil :corpora [probe taler] :folders []}]
             (registry/grouped-corpora [{:label   "Litteratur"
                                         :corpora ["GONE"]
                                         :folders [{:label   "Empty"
                                                    :corpora ["X"]}
                                                   {:label   "Folkeviser"
                                                    :corpora ["VISER"]}]}
                                        {:label "Nothing" :corpora []}]
                                       overviews))))))
