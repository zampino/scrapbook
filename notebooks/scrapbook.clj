(ns scrapbook
  {:nextjournal.clerk/visibility {:code :hide}
   :nextjournal.clerk/no-cache true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]))

(clerk/html
  {::clerk/width :full}
  [:div.p-10.flex.flex-col.items-center
   [:h1 "A Scrapbook"]
   [:em "Ritagli sparsi"]
   [:div.flex.flex-col.mt-2.items-center
    [:a.font-sans {:href (clerk/doc-url "notebooks/scrapbook/deputy/cantor")} "A proof of Cantor theorem in Clojure with Deputy"]
    [:a.font-sans {:href (clerk/doc-url "notebooks/scrapbook/deputy/fold_universal")} "The universality of fold"]
    [:a.font-sans {:href (clerk/doc-url "notebooks/scrapbook/zippers/scars")} "Zippers with Scars à la Huet"]
    [:a.font-sans {:href (clerk/doc-url "notebooks/scrapbook/tests")} "Clerk as test runner"]]])
