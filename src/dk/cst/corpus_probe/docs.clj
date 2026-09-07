(ns dk.cst.corpus-probe.docs
  "The documents the app renders from Markdown: the frontpage, the
  search help, the CQP guide and the glossary, one file per language
  under resources/docs (`help.da.md` beside `help.en.md`), parsed on the
  server into the hiccup the views render, so the client needs no
  parser. CommonMark plus a definition list (see
  dk.cst.corpus-probe.docs.markdown); raw HTML renders as nothing."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [dk.cst.corpus-probe.docs.markdown :as markdown]
            [dk.cst.corpus-probe.hiccup :as hiccup]))

(defn resource
  "The Markdown file of document `name` on the classpath in the first of
  the language codes `langs` that has one; nil when none has."
  [name langs]
  (some #(io/resource (str "docs/" name "." % ".md")) langs))

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
  (or (hiccup/heading? x) (and (vector? x) (= :dt (first x)))))

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

(defn blocks
  "Markdown text `s` as hiccup blocks, each heading and each term of a
  definition list carrying the id it names (see `name-node`)."
  [s]
  (walk/postwalk (fn [x] (if (nameable? x) (name-node x) x))
                 (rest (markdown/->hiccup s))))

(defn title
  "The text of the first heading among the hiccup `blocks`, which is what
  a document calls itself; nil without one."
  [blocks]
  (some #(when (hiccup/heading? %) (hiccup/heading-text %)) blocks))

(defn document
  "The hiccup blocks of document `name` in the first language of `langs`
  that has a file (see `resource`), or nil without one.

  Read on every call, so an edited file is served as edited; parsing
  costs about a millisecond."
  [name langs]
  (some-> (resource name langs) slurp blocks))

(comment
  (document "help" ["da"])
  (document "cqp-guide" ["xx" "en"])
  (blocks "# A\n\n<!-- dropped -->\n\nsome *prose*")
  ;; => ([:h1 {:id "a"} "A"] [:p "some " [:em "prose"]])

  (blocks "## Konkordans {#kwic}")
  ;; => ([:h2 {:id "kwic"} "Konkordans"])

  (blocks "KWIC {#kwic}:\n  key word in context")
  ;; => ([:dl [:dt {:id "kwic"} "KWIC"] [:dd "key word in context"]])
  #_.)
