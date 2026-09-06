(ns dk.cst.corpus-probe.markdown-test
  (:require [clojure.test :refer [deftest is testing]]
            [dk.cst.corpus-probe.markdown :as markdown]))

(deftest definition-list-test
  (testing "a term ending in a colon, its definition indented under it"
    (is (= [:div [:dl [:dt "term"] [:dd "definition"]]]
           (markdown/->hiccup "term:\n  definition"))))
  (testing "pair after pair is one list, and so are lists a blank line apart"
    (is (= [:div [:dl [:dt "a"] [:dd "1"] [:dt "b"] [:dd "2"]]]
           (markdown/->hiccup "a:\n  1\nb:\n  2")))
    (is (= [:div [:dl [:dt "a"] [:dd "1"] [:dt "b"] [:dd "2"]]]
           (markdown/->hiccup "a:\n  1\n\nb:\n  2"))))
  (testing "a definition may run over lines, and both parts take inline markup"
    (is (= [:div [:dl [:dt [:code "x"] " or " [:em "y"]] [:dd "one" " " "two"]]]
           (markdown/->hiccup "`x` or *y*:\n  one\n  two")))
    (is (= [:div [:dl [:dt "a"] [:dd "one" " " "two" " " "three" " " "four"]]]
           (markdown/->hiccup "a:\n  one\n  two\n  three\n  four"))))
  (testing "a term may hold a colon of its own; the last one is the mark"
    (is (= [:div [:dl [:dt [:code "a:[] :: b"]] [:dd "labels"]]]
           (markdown/->hiccup "`a:[] :: b`:\n  labels"))))
  (testing "the list ends at a blank line"
    (is (= [:div [:dl [:dt "a"] [:dd "1"]] [:p "after"]]
           (markdown/->hiccup "a:\n  1\n\nafter"))))
  (testing "a paragraph is all pairs or no list: a stray line, or a term
            without a definition, leaves the prose it was"
    (is (= [:div [:p "a:" " " "1" " " "after"]]
           (markdown/->hiccup "a:\n  1\nafter")))
    (is (= [:div [:p "a:" " " "1" " " "b:"]]
           (markdown/->hiccup "a:\n  1\nb:"))))
  (testing "inside a list item, indentation counts from the item"
    (is (= [:div [:ul [:li [:dl [:dt "a"] [:dd "1"]]]]]
           (markdown/->hiccup "- a:\n    1")))))

(deftest prose-test
  (testing "a colon with nothing indented under it is prose, not a term"
    (is (= [:div [:p "For example:"] [:pre [:code "x\n"]]]
           (markdown/->hiccup "For example:\n\n```\nx\n```")))
    (is (= [:div [:p "Note:"]] (markdown/->hiccup "Note:"))))
  (testing "and a wrapped paragraph whose line ends in one is still one paragraph"
    (is (= [:div [:p "The views are these:" " " "the KWIC and the frequencies." " " "Both count."]]
           (markdown/->hiccup "The views are these:\nthe KWIC and the frequencies.\nBoth count.")))
    (is (= [:div [:p "a:" " " "b:"]] (markdown/->hiccup "a:\nb:"))))
  (testing "such a paragraph still yields to a block that may interrupt one"
    (is (= [:div [:p "Note:"] [:h1 {:id "h"} "H"]]
           (markdown/->hiccup "Note:\n# H")))
    (is (= [:div [:p "Note:"] [:ul [:li ["x"]]]]
           (markdown/->hiccup "Note:\n- x"))))
  (testing "a line inside a paragraph is never a term"
    (is (= [:div [:p "one" " " "two:" " " "three"]]
           (markdown/->hiccup "one\ntwo:\n  three"))))
  (testing "nor is one that begins as another block does"
    (is (= [:div [:h2 {:id "heading:"} "Heading:"] [:p "x"]]
           (markdown/->hiccup "## Heading:\n  x")))))

(deftest everything-else-test
  (testing "the library's own constructs parse as before"
    (is (= [:div [:h1 {:id "t"} "T"] [:p "a " [:a {:href "/x"} "link"] " and " [:code "c"]]
            [:ul [:li ["one"]]] [:table [:thead [:tr [:th "a"]]] [:tbody [:tr [:td "1"]]]]]
           (markdown/->hiccup "# T\n\na [link](/x) and `c`\n\n- one\n\n| a |\n|---|\n| 1 |"))))
  (testing "raw HTML renders as nothing"
    (is (= [:div [:p "b " "c"]] (markdown/->hiccup "<!-- x -->\n\nb <b>c</b>"))))
  (testing "a dollar sign is a dollar sign, not a formula"
    (is (= [:div [:p "$5 and $6"]] (markdown/->hiccup "$5 and $6")))))

(deftest keys-test
  (testing "a key in double brackets is the element for keyboard input,
            and a chord one for each key inside one for the whole"
    (is (= [:div [:p "press " [:kbd "Enter"] "."]]
           (markdown/->hiccup "press [[Enter]].")))
    (is (= [:div [:p [:kbd [:kbd "Shift"] "+" [:kbd "Enter"]]]]
           (markdown/->hiccup "[[Shift+Enter]]")))
    (is (= [:div [:p [:kbd "+"]]] (markdown/->hiccup "[[+]]"))))
  (testing "not inside code"
    (is (= [:div [:p [:code "[[x]]"]]] (markdown/->hiccup "`[[x]]`"))))
  (testing "a label on the screen in double braces is a sample of the
            screen inside keyboard input"
    (is (= [:div [:p "click " [:kbd [:samp "Search"]]]]
           (markdown/->hiccup "click {{Search}}")))))
