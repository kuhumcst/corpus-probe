(ns dk.cst.corpus-probe.query.tokens-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.query :as query]
            [dk.cst.corpus-probe.query.tokens :as tokens]))

(deftest token-field-test
  (testing "a token's fields are read from their key"
    (is (= [2 1 :v] (tokens/token-field :t2.v)))
    (is (= [2 3 :join] (tokens/token-field :t2.3.join)))
    (is (nil? (tokens/token-field :t2.x)))
    (is (nil? (tokens/token-field :tv)))
    (is (nil? (tokens/token-field nil)))
    (is (tokens/token-key? :t1.attr))
    (is (not (tokens/token-key? :within))))
  (testing "and a key is built as it is read"
    (is (= "t2.v" (tokens/token-key 2 1 :v)))
    (is (= "t2.3.join" (tokens/token-key 2 3 :join)))
    (is (= [2 3 :join]
           (tokens/token-field (keyword (tokens/token-key 2 3 :join)))))
    (is (= {:id 4 :conditions [{:id 1}]} (tokens/blank-token 4)))))

(deftest token-rows-test
  (testing "the tokens in order, each with its own fields and its
            conditions in order"
    (is (= [{:n 1 :conditions [{:c 1 :v "hund" :ci "on"}]}
            {:n 3 :max "2" :start "on" :conditions [{:c 1 :op "any"}]}]
           (tokens/token-rows {:t3.op "any" :t1.v "hund" :t3.max "2"
                               :t3.start "on" :t1.ci "on" :q "x"})))
    (is (= [{:n 1 :conditions [{:c 1 :v "a"} {:c 2 :v "b" :join "or"}]}]
           (tokens/token-rows {:t1.2.join "or" :t1.v "a" :t1.2.v "b"})))))

(deftest asks?-test
  (testing "a token asks for something when one of its conditions does:
            with a value, or as any word"
    (is (tokens/asks? {:conditions [{:v "hund"}]}))
    (is (tokens/asks? {:conditions [{:op "any"}]}))
    (is (tokens/asks? {:conditions [{:v ""} {:v "b" :join "or"}]}))
    (is (not (tokens/asks? {:conditions [{:op "prefix" :ci "on"}]})))
    (is (not (tokens/asks? {:conditions [{:v ""}]})))))

(deftest present-test
  (is (= "x" (tokens/present "x")))
  (is (= "2" (tokens/present 2)))
  (is (nil? (tokens/present "  ")))
  (is (nil? (tokens/present nil)))
  (testing "a vector keeps what says something, and is nothing without any"
    (is (= ["a" "b"] (tokens/present ["a" "" "b"])))
    (is (nil? (tokens/present ["" " "])))))

(deftest rows->params-test
  (testing "the rows print as the params the form submits, and read back
            as themselves"
    (let [rows [{:id 1 :conditions [{:id 1 :v "a"}
                                    {:id 2 :v "c" :join "or" :attr "pos"}]}
                {:id 2 :max "2" :start "on" :conditions [{:id 1 :op "any"}]}]]
      (is (= {:t1.v "a" :t1.2.v "c" :t1.2.join "or" :t1.2.attr "pos"
              :t2.max "2" :t2.start "on" :t2.op "any"}
             (tokens/rows->params rows)))
      (is (= rows (tokens/form-tokens
                   (tokens/token-rows (tokens/rows->params rows)))))
      (is (= {} (tokens/rows->params []))))))

(deftest own-rows-test
  (testing "the blank last token the server ends the rows in goes"
    (is (= [{:id 1 :conditions [{:id 1 :v "a"}]}]
           (tokens/own-rows [{:id 1 :conditions [{:id 1 :v "a"}]}
                             {:id 2 :conditions [{:id 1}]}]))))
  (testing "unless it is the only one, or asks for something"
    (is (= [{:id 1 :conditions [{:id 1}]}]
           (tokens/own-rows [{:id 1 :conditions [{:id 1}]}])))
    (is (= [{:id 1 :conditions [{:id 1 :v "a"}]}
            {:id 2 :conditions [{:id 1 :op "any"}]}]
           (tokens/own-rows [{:id 1 :conditions [{:id 1 :v "a"}]}
                             {:id 2 :conditions [{:id 1 :op "any"}]}])))))

(deftest vocabularies-test
  (testing "the match ops are the compiler's literal operators, the whole
            form standing first as the value no URL carries"
    (is (= "" (first tokens/match-ops)))
    (is (= (disj query/literal-ops "is") (set (rest tokens/match-ops)))))
  (testing "every operator and join is a string a form can submit"
    (is (every? string? tokens/operators))
    (is (every? string? tokens/joins))))

(deftest token-defaults-test
  (testing "the defaults are the compiler's"
    (is (= "[word = \"x\"]"
           (query/extended->cqp
            [{:conditions [{:attr  (keyword (:attr tokens/token-defaults))
                            :op    (:op tokens/token-defaults)
                            :value "x"}]
              :min        (parse-long (:min tokens/token-defaults))
              :max        (parse-long (:max tokens/token-defaults))}])))
    (is (= "[a = \"x\" & b = \"y\"]"
           (query/token->cqp
            {:conditions [{:attr :a :value "x"}
                          {:attr :b :value "y"
                           :join (:join tokens/token-defaults)}]})))))
