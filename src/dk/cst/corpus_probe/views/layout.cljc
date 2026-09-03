(ns dk.cst.corpus-probe.views.layout
  "Hiccup shared by every page: the site masthead with its navigation.")

(defn site-header
  "The site masthead shared by every page: the name linking home, and the
  top-level navigation."
  []
  [:header
   [:h1 [:a {:href "/"} "corpus-probe"]]
   [:p.subtitle "CWB corpus search"]
   [:nav {:aria-label "Site"}
    [:ul
     [:li [:a {:href "/"} "search"]]
     [:li [:a {:href "/frequencies"} "frequencies"]]
     [:li [:a {:href "/corpora"} "corpora"]]]]])
