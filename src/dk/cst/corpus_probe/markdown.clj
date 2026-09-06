(ns dk.cst.corpus-probe.markdown
  "Markdown for the documents under resources/docs: nextjournal's
  markdown library (commonmark-java underneath) plus a definition list,
  which CommonMark lacks, and notations for keys and for the labels on
  the screen a reader presses (see `keys-tokenizer` and
  `labels-tokenizer`).

      term:
        the definition, over as many
        indented lines as it takes

  Pairs one after another, or a blank line apart, are one list. A
  paragraph is a list only when it is nothing but such pairs, so a line
  ending in a colon with prose after it stays prose.

  The list is found after parsing: commonmark-java never tries block
  starts on a line beginning with a letter, so a block parser would not
  see `term:`. A post-processor sees the paragraph, and the source spans
  say how far each line was indented. The parser is rebuilt here with
  the library's own extensions, formulas aside, because the library's
  builder takes no post-processor; its tree goes back to the library for
  the data and the hiccup."
  (:require [clojure.string :as str]
            [nextjournal.markdown.impl :as impl]
            [nextjournal.markdown.impl.utils :as u]
            [nextjournal.markdown.transform :as transform])
  (:import (org.commonmark.ext.autolink AutolinkExtension)
           (org.commonmark.ext.footnotes FootnotesExtension)
           (org.commonmark.ext.gfm.strikethrough StrikethroughExtension)
           (org.commonmark.ext.gfm.tables TablesExtension)
           (org.commonmark.ext.task.list.items TaskListItemsExtension)
           (org.commonmark.node CustomBlock HardLineBreak Node Paragraph
                                SoftLineBreak Text)
           (org.commonmark.parser IncludeSourceSpans Parser PostProcessor)))

;; Three custom blocks with a marker interface each: the library converts
;; a node by its class, and a proxy's class is its superclass plus
;; interfaces, so without the markers all three would be one class.

(definterface DefinitionList)
(definterface DefinitionTerm)
(definterface DefinitionDetail)

(defn ->list [] (proxy [CustomBlock DefinitionList] []))
(defn ->term [] (proxy [CustomBlock DefinitionTerm] []))
(defn ->detail [] (proxy [CustomBlock DefinitionDetail] []))

(defn children
  "The child nodes of commonmark `node`, in order."
  [^Node node]
  (take-while some? (iterate (fn [^Node n] (.getNext n)) (.getFirstChild node))))

(defn line-break?
  "True when inline `node` is the break between two source lines."
  [node]
  (or (instance? SoftLineBreak node) (instance? HardLineBreak node)))

(defn lines
  "The inline `nodes` of a paragraph as the source lines they came from:
  a vector of the nodes of each line, the breaks between them left out."
  [nodes]
  (->> (partition-by line-break? nodes)
       (remove (comp line-break? first))
       (mapv vec)))

(defn column
  "The column the first of the `nodes` of a line starts in, from its
  source span; nil without one."
  [nodes]
  (some-> ^Node (first nodes) .getSourceSpans first .getColumnIndex))

(defn term-line?
  "True when the `nodes` of a line end in a colon."
  [nodes]
  (let [end (peek nodes)]
    (and (instance? Text end)
         (str/ends-with? (str/trimr (.getLiteral ^Text end)) ":"))))

(defn pairs
  "The `lines` of a paragraph starting in column `base` as definition
  pairs, [term-nodes definition-lines] each; nil unless every line is a
  term at `base` ending in a colon or a definition indented two or more
  columns under one, and every term has a definition."
  [base lines]
  (loop [lines lines
         acc   []]
    (if (empty? lines)
      (not-empty acc)
      (let [[term & more] lines]
        (when (and (= base (column term)) (term-line? term))
          (let [[definition after] (split-with #(<= (+ base 2) (or (column %) -1))
                                               more)]
            (when (seq definition)
              (recur after (conj acc [term (vec definition)])))))))))

(defn without-colon!
  "The `nodes` of a term with the colon taken off the end of its last
  text, which goes too when nothing is left of it."
  [nodes]
  (let [^Text end (peek nodes)
        text      (str/trimr (.getLiteral end))
        text      (subs text 0 (dec (count text)))]
    (if (str/blank? text)
      (do (.unlink end) (pop nodes))
      (do (.setLiteral end text) nodes))))

(defn adopt!
  "Move the `nodes` into `parent`, in order; returns `parent`."
  [^Node parent nodes]
  (doseq [^Node node nodes]
    (.unlink node)
    (.appendChild parent node))
  parent)

(defn with-breaks
  "The inline nodes of the `lines` of a definition, one line after
  another with a soft break between each two, a new node each: a node
  has one place in the tree, so one break could not stand in three."
  [lines]
  (apply concat (butlast (interleave lines
                                     (repeatedly #(vector (SoftLineBreak.)))))))

(defn definition-list!
  "The `pairs` of a paragraph as a definition list node, the paragraph's
  own inline nodes moved into it: each term without its colon, each
  definition's lines with the breaks between them put back."
  [pairs]
  (let [dl (->list)]
    (doseq [[term definition] pairs]
      (.appendChild dl (adopt! (->term) (without-colon! term)))
      (.appendChild dl (adopt! (->detail) (with-breaks definition))))
    dl))

(def definition-lists
  "The post-processor: every paragraph that is nothing but definition
  pairs becomes a definition list in its place."
  (reify PostProcessor
    (process [_ document]
      (doseq [^Paragraph para (vec (filter #(instance? Paragraph %)
                                           (tree-seq (constantly true)
                                                     children document)))]
        (let [ls   (lines (vec (children para)))
              base (column (first ls))]
          (when-let [pairs (and base (next ls) (pairs base ls))]
            (.insertAfter para (definition-list! pairs))
            (.unlink para))))
      document)))

(def parser
  "The commonmark parser: the library's own extensions but its formulas,
  source spans for the post-processor to read indentation from, and the
  definition list."
  (-> (Parser/builder)
      (.extensions [(AutolinkExtension/create)
                    (TaskListItemsExtension/create)
                    (TablesExtension/create)
                    (StrikethroughExtension/create)
                    (-> (FootnotesExtension/builder)
                        (.inlineFootnotes true)
                        (.build))])
      (.includeSourceSpans IncludeSourceSpans/BLOCKS_AND_INLINES)
      (.postProcessor definition-lists)
      (.build)))

;; how the library reads the three nodes into its data: a container each

(defmethod impl/open-node (class (->list)) [ctx _node]
  (u/update-current-loc ctx #(u/zopen-node % {:type :definition-list
                                              :content []})))

(defmethod impl/open-node (class (->term)) [ctx _node]
  (u/update-current-loc ctx #(u/zopen-node % {:type :definition-term
                                              :content []})))

(defmethod impl/open-node (class (->detail)) [ctx _node]
  (u/update-current-loc ctx #(u/zopen-node % {:type :definition-detail
                                              :content []})))

(defn merge-lists
  "The document `content` (its block nodes) with adjacent definition
  lists joined into one: a blank line between two pairs ends the
  paragraph the parser sees, and should not end the list the reader
  sees."
  [content]
  (reduce (fn [acc node]
            (if (and (= :definition-list (:type node))
                     (= :definition-list (:type (peek acc))))
              (conj (pop acc) (update (peek acc) :content into (:content node)))
              (conj acc node)))
          []
          content))

(def keys-tokenizer
  "A key or a chord of keys in double brackets, `[[Enter]]` or
  `[[Shift+Enter]]`, read out of the text for the element HTML has for
  keyboard input (see `keys->hiccup`), which raw HTML cannot give, being
  dropped (see `renderers`). The library's own use of the brackets, for
  a link within a site, is not in play here."
  {:regex   #"\[\[([^\]]+)\]\]"
   :handler (fn [match] {:type :kbd :text (second match)})})

(def labels-tokenizer
  "A label the screen shows and the reader presses, a button's or a
  menu's, in double braces, `{{Search}}`, read out of the text for the
  element HTML has for such input, a sample of the screen's output
  inside keyboard input (see `label->hiccup`)."
  {:regex   #"\{\{([^}]+)\}\}"
   :handler (fn [match] {:type :label :text (second match)})})

(defn parse
  "Markdown text `s` as the library's document data, definition lists,
  keys and labels included."
  [s]
  (-> (impl/node->data (assoc-in u/empty-doc [:opts :text-tokenizers]
                                 (mapv u/normalize-tokenizer
                                       [keys-tokenizer labels-tokenizer]))
                       (.parse parser s))
      (update :content merge-lists)))

(defn keys->hiccup
  "The hiccup of a `node` of keys (see `keys-tokenizer`): a kbd element
  for the key, or, for a chord, one for each key inside one for the
  whole, the plus signs between them as text, which is how HTML marks
  keys pressed together."
  [_ctx {:keys [text]}]
  ;; a plus between two characters divides keys; a key of its own does not
  (let [keys (map (fn [k] [:kbd k]) (str/split text #"(?<=.)\+(?=.)"))]
    (if (next keys)
      (into [:kbd] (interpose "+" keys))
      (first keys))))

(defn label->hiccup
  "The hiccup of a label `node` (see `labels-tokenizer`): a samp element
  for the words the screen shows, inside a kbd element for pressing
  them, which is how HTML marks input given through what is on the
  screen."
  [_ctx {:keys [text]}]
  [:kbd [:samp text]])

(def renderers
  "The hiccup renderers: the library's own, the definition list as the
  elements HTML has for it, the keys and the labels as theirs, and raw
  HTML as nothing rather than the library's error message."
  (assoc transform/default-hiccup-renderers
         :definition-list   (partial transform/into-markup [:dl])
         :definition-term   (partial transform/into-markup [:dt])
         :definition-detail (partial transform/into-markup [:dd])
         :kbd               keys->hiccup
         :label             label->hiccup
         :html-block        (constantly nil)
         :html-inline       (constantly nil)))

(defn ->hiccup
  "Markdown text `s` as hiccup, one root element over the blocks."
  [s]
  (transform/->hiccup renderers (parse s)))

(comment
  (->hiccup "term:\n  definition\n\nother:\n  with `code`")
  ;; => [:div [:dl [:dt "term"] [:dd "definition"]
  ;;           [:dt "other"] [:dd "with " [:code "code"]]]]

  (->hiccup "For example:\n\n```\nx\n```")
  ;; => [:div [:p "For example:"] [:pre [:code "x\n"]]]

  (->hiccup "press [[Shift+Enter]] or {{Search}}")
  ;; => [:div [:p "press " [:kbd [:kbd "Shift"] "+" [:kbd "Enter"]]
  ;;           " or " [:kbd [:samp "Search"]]]]
  #_.)
