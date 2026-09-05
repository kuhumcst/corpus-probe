(ns dk.cst.corpus-probe.docs
  "The documents the app renders from Markdown: the frontpage, the
  glossary and the search guide, one file per language under
  resources/docs (`guide.da.md` beside `guide.en.md`), parsed on the
  server into the hiccup the views render, so the client needs no
  parser. CommonMark plus a definition list (see
  dk.cst.corpus-probe.markdown); raw HTML renders as nothing."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [dk.cst.corpus-probe.markdown :as markdown]))

(defn resource
  "The Markdown file of document `name` on the classpath in the first of
  the language codes `langs` that has one; nil when none has."
  [name langs]
  (some #(io/resource (str "docs/" name "." % ".md")) langs))

(def headings
  "The heading tags, outermost first."
  [:h1 :h2 :h3 :h4 :h5 :h6])

(defn heading?
  "True when hiccup `x` is a heading."
  [x]
  (and (vector? x) (boolean (some #{(first x)} headings))))

(def explicit-id
  "How a heading or a term names its own id, at the end of its text:
  `## KWIC {#kwic}`, the header attribute syntax of Pandoc and kramdown.

  So an entry keeps one id in every language, and a link into a document
  does not depend on how a translation words its heading."
  #"\s*\{#([\w-]+)\}\s*$")

(defn nameable?
  "True when hiccup `x` may name its own id: a heading, or the term of a
  definition list."
  [x]
  (or (heading? x) (and (vector? x) (= :dt (first x)))))

(defn name-node
  "The hiccup `h` with the id its text names (see `explicit-id`) in place
  of any it carries, and the naming taken out of the text; `h` as it is
  when it names none."
  [[tag & more :as h]]
  (let [[attrs children] (if (map? (first more))
                           [(first more) (rest more)]
                           [{} more])
        text (last children)
        id   (when (string? text) (second (re-find explicit-id text)))]
    (if id
      (into [tag (assoc attrs :id id)]
            (concat (butlast children) [(str/replace text explicit-id "")]))
      h)))

(defn ->hiccup
  "Markdown text `s` as hiccup blocks, each heading and each term of a
  definition list carrying the id it names (see `name-node`)."
  [s]
  (walk/postwalk (fn [x] (if (nameable? x) (name-node x) x))
                 (rest (markdown/->hiccup s))))

(defn title
  "The text of the first heading among the hiccup `blocks`, which is what
  a document calls itself; nil without one."
  [blocks]
  (some (fn [[_ _ & children :as x]]
          (when (heading? x)
            (apply str (filter string? (tree-seq coll? seq children)))))
        blocks))

(defn hiccup
  "The hiccup of document `name` in the first language of `langs` that
  has a file (see `resource`), or nil without one.

  Read on every call, so an edited file is served as edited; parsing
  costs about a millisecond."
  [name langs]
  (some-> (resource name langs) slurp ->hiccup))

(def heading-below
  "Each heading tag and the one a level below it; an h6 has none."
  (zipmap headings (rest headings)))

(defn demote
  "The hiccup `blocks` with every heading one level lower, for a document
  shown under a page's own heading: the document's `#` becomes the
  page's h2."
  [blocks]
  (walk/postwalk (fn [x]
                   (if (heading? x) (update x 0 #(heading-below % %)) x))
                 blocks))

(comment
  (hiccup "guide" ["da"])
  (demote (hiccup "guide" ["xx" "en"]))
  (->hiccup "# A\n\n<!-- dropped -->\n\nsome *prose*")
  ;; => ([:h1 {:id "a"} "A"] [:p "some " [:em "prose"]])

  (->hiccup "## Konkordans {#kwic}")
  ;; => ([:h2 {:id "kwic"} "Konkordans"])

  (->hiccup "KWIC {#kwic}:\n  key word in context")
  ;; => ([:dl [:dt {:id "kwic"} "KWIC"] [:dd "key word in context"]])
  #_.)
