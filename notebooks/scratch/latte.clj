(ns scratch.latte
  #_ {:nextjournal.clerk/no-cache true}
  (:require [latte.core :refer [definition]]
            [latte-kernel.typing]
            [scratch.macros :refer [dep-def]]
            [nextjournal.clerk.analyzer :as clerk.analyzer]
            [nextjournal.clerk.analyzer :as analyzer]
            [nextjournal.clerk.parser :as parser]

            [nextjournal.clerk :as clerk]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.jvm :as ana-jvm]
            [clojure.tools.analyzer.utils :as ana-utils]))

(dep-def X)
(dep-def Y)
(dep-def Z Y X)

(comment
  (analyzer/analyze-doc
   (parser/parse-file {:doc? true} "notebooks/scratch/latte.clj"))

  (analyzer/analyze '(dep-def X))
  (analyzer/analyze '(dep-def Y X))

  (ex-data *e)
  (def ana *1)
  (->  ana :->analysis-info (get `Y) )
  (->  ana keys )

  (ns-publics *ns*)
  (->> (ns-publics *ns*) keys (map (partial ns-unmap *ns*)))
  @(resolve 'C)


  (do *e)
  (resolve 'clojure.set/rename-keys)
  (ns-unmap *ns* 'dep-def)

  (clerk.analyzer/analyze-doc (nextjournal.clerk.parser/parse-file "notebooks/scratch/latte.clj"))

  ;; original
  (definition Set
    "Sets with elements in the type T"
    [[T :type]]
    (==> T :type))

  (definition Powerset
    "Collections of sets"
    [[T :type]]
    (==> (Set T) :type))

  (do *e)
  (ns-unmap *ns* 'Set)
  (ns-unmap *ns* 'Powerset)

  (macroexpand
   '(definition Set
      "Sets with elements in the type T"
      [[T :type]]
      (==> T :type)))

  (nextjournal.clerk.analyzer/analyze
   '(definition Set
      "Sets with elements in the type T"
      [[T :type]]
      (==> T :type)))

  (macroexpand
   '(definition Powerset
      "Collections of sets"
      [[T :type]]
      (==> (Set T) :type)))

  (nextjournal.clerk.analyzer/analyze
   '(definition Powerset
      "Collections of sets"
      [[T :type]]
      (==> (Set T) :type)))

  (def bar +)
  (clerk.analyzer/analyze
   '(def foo (bar 1 3))))
