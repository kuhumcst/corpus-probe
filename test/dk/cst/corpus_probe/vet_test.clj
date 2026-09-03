(ns dk.cst.corpus-probe.vet-test
  "The startup self-checks: the CWB programs, the sort collation, and what
  the registry's corpora actually read as."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.corpus-test :refer [fixture]]
            [dk.cst.corpus-probe.cqp-test :refer [ctx temp-registry when-cwb]]
            [dk.cst.corpus-probe.vet :as vet]))

(defn phantom-ctx
  "A context over a fresh registry holding one entry whose HOME points at
  data that is not there, which is what the three phantom KU entries look
  like.

  Fresh per call: the per-corpus facts cache is keyed by the registry path
  and the entry's mtime, so two checks of one registry would share the
  first one's outcome."
  []
  {:registry (temp-registry fixture {})})

(deftest installed?-test
  (is (vet/installed? "echo"))
  (testing "a command this machine does not have is not installed"
    (is (not (vet/installed? "no-such-command-xyzzy"))))
  (testing "a non-zero exit still counts, as every cwb-* tool gives one"
    (is (vet/installed? "false"))))

(deftest tools!-test
  (when-cwb
   (testing "an installation the suite already runs against is complete"
     (is (empty? (vet/tools! ctx))))
   (testing "a cqp that is not there is named"
     (is (= ["no-such-cqp"] (vet/tools! (assoc ctx :cqp "no-such-cqp")))))))

(deftest collation-test
  (when-cwb
   (testing "under a Danish locale CQP's pipeline and our collator agree"
     (is (= (vet/pipeline-order "da_DK.UTF-8")
            (vet/collator-order "da_DK.UTF-8")))
     (is (empty? (vet/collation-problems "da_DK.UTF-8"))))
   (testing "and they order æ, ø and å after z, not in byte order"
     (is (= ["4" "2" "1" "5" "3"] (vet/pipeline-order "da_DK.UTF-8"))))
   (testing "a locale this machine does not have collates by bytes instead"
     (is (= ["4" "2" "3" "1" "5"] (vet/pipeline-order "zz_ZZ.UTF-8")))
     (is (= [:collation-mismatch]
            (vet/collation-problems "zz_ZZ.UTF-8"))))
   (testing "so does asking for byte order outright"
     (is (= [:collation-mismatch] (vet/collation-problems "C")))))
  (testing "no configured locale is a problem of its own, not a pass"
    (is (= [:sort-locale-unset] (vet/collation-problems nil)))
    (is (= [:sort-locale-unset] (vet/collation-problems "")))))

(deftest collation-problems-test
  (testing "a pipeline that cannot run at all is told apart from a mismatch"
    (with-redefs [vet/pipeline-order (constantly nil)]
      (is (= [:pipeline-broken] (vet/collation-problems "da_DK.UTF-8"))))))

(deftest corpus!-test
  (when-cwb
   (testing "a corpus CWB can read vets clean"
     (is (nil? (vet/corpus! ctx {:id "probe"}))))
   (testing "an entry CWB has no data for is named, uppercase, as undefined"
     (is (= ["PROBE" :undefined] (vet/corpus! (phantom-ctx) {:id "probe"}))))
   (testing "another failure carries its type, which names no server path"
     (let [[id reason] (vet/corpus! (assoc (phantom-ctx) :cqp "no-such-cqp")
                                    {:id "probe"})]
       (is (= "PROBE" id))
       (is (contains? #{:unreadable :timeout :cqp :misaligned} reason))
       (is (not= :undefined reason))))))

(deftest registry!-test
  (when-cwb
   (testing "a registry of nothing but a phantom reports it"
     (is (= [["PROBE" :undefined]] (vet/registry! (phantom-ctx)))))
   (testing "the dev corpora read clean"
     ;; naming them, since the gitignored dev registry may hold more
     (let [broken (set (map first (vet/registry! ctx)))]
       (is (not-any? broken ["PROBE" "VISER" "TALER"])))))
  (testing "a registry path that names nothing reads as empty, not as fine"
    ;; a mistyped :registry looks exactly like an empty one, so the caller
    ;; gets no corpora rather than an error
    (is (empty? (vet/registry! {:registry "/no/such/registry"})))))
