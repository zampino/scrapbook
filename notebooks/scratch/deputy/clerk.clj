(ns scratch.deputy.clerk
  (:refer-clojure :exclude [set])
  (:require
   [deputy.core :as d]
   [deputy.syntax :as s :refer :all]
   [deputy.ast :as a]
   [deputy.unparse :as u]
   [deputy.stdlib.defdata :refer [defdata]]
   [deputy.norm :as n]
   [deputy.typing :as t]
   [nextjournal.clerk :as clerk]
   [nextjournal.clerk.viewer :as v]))

(def term-viewer
  {:pred (every-pred map? :def-id :node :term :vtype)
   ;;:render-fn '(fn [{:keys [def-id type]}] [:div.green-200 def-id " : " type])
   :transform-fn (comp v/mark-presented
                       (v/update-val (fn [{:keys [def-id vtype]}]
                                       (v/html
                                        [:div.flex.items-center
                                         (v/code (name def-id))
                                         [:span.mx-3 ":"]
                                         (v/code (a/unparse (n/evaluate vtype)))
                                         ]))))})

(d/defterm [set [A :type] :type] (=> A :type))
(d/defterm [powerset [A :type] :type] (=> (set A) :type))

(v/with-viewer term-viewer set)
(v/with-viewer term-viewer powerset)

(d/defterm [A :type] :unit)
(comment
  (t/type-synth
   (s/parse (=> (set A) :type)))

  (a/unparse (t/type-synth powerset))

  (-> set :vtype a/unparse)
  (-> set :vtype)
  (-> powerset :vtype n/evaluate a/unparse)
  (-> powerset :vtype)
  set
  (doseq [[n _] (ns-publics *ns*)] (ns-unmap *ns* n)))

(defdata ⊕ [A :type B :type]
  (injl [a A])
  (injr [b B]))

(d/defterm [⊕-sym [A :type] [B :type]
            [ab (⊕ A B)] (⊕ B A)]
  (⊕-case A B ab
          ;;?left
          ;;?right
          ;;#_#_
          (λ [_] (⊕ B A))
          (λ [a] (injr B A a))
          (λ [b] (injl B A b))))

(comment
  (clojure.tools.analyzer.jvm/macroexpand-all
   '(d/defterm [⊕-sym [A :type] [B :type]
                [ab (⊕ A B)] (⊕ B A)]
      (⊕-case A B ab
              (λ [_] (⊕ B A))
              (λ [a] (injr B A a))
              (λ [b] (injl B A b)))))

  (nextjournal.clerk.analyzer/analyze
   '(defdata ⊕ [A :type B :type]
      (injl [a A])
      (injr [b B])))

  (-> (nextjournal.clerk.parser/parse-file "notebooks/scratch/deputy/clerk.clj")
      (nextjournal.clerk.analyzer/build-graph)
      :->analysis-info keys )

  (nextjournal.clerk.analyzer/analyze
   '(d/defterm [hey :type] :unit))
  (macroexpand
   '(d/defterm [hey :type] :unit))

  (ns-publics *ns*)
  (doseq [[n _] (ns-publics *ns*)] (ns-unmap *ns* n))
  )
