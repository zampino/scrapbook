;; # Logic
(ns scratch.deputy.logic
  (:require
   [deputy.core :as d]
   [deputy.syntax :as s :refer :all]
   [deputy.ast :as a]
   [deputy.unparse :as u]
   [deputy.stdlib.bottom ]
   [deputy.extensions.enum :as e]
   [deputy.stdlib.defdata :refer [defdata]]
   [deputy.norm :as n]
   [nextjournal.clerk :as clerk]))

;; ## Sum (disjunction) and Product (conjunction)
;;
;;        A
;;      /   \
;;    AxB   A⊕B
;;      \   /
;;        B
;; ---
(d/defterm [× [A :type] [B :type] :type] (Σ [x A] B))

(d/defterm [implies-pair [A :type] [B :type]
            [a A] [b B] (× A B)]
  (pair a b))

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

;; can't leave holes like this?
#_
(d/defterm [×-sym [A :type] [B :type]
            [ab (× A B)] (× B A)]
  (implies-pair B A (π2 ab) (π1 ?p)))

(d/defterm [×-sym [A :type] [B :type]
            [ab (× A B)] (× B A)]
  (implies-pair B A (π2 ab) (π1 ab)))

(comment
  (s/parse (implies-pair B A (π2 ab) (π1 ab)))


  (ns-publics *ns*)
  (ex-data *e))


;; ## The empty type ⟂
(d/defterm [⟂ :type] (e/enum))
(d/defterm [¬ [A :type] :type] (=> A ⟂))

#_
(d/defterm [⟂-elim [A :type] [_ ⟂] A]
  (e/switch ))

;; non contraddiction



(ex-data *e)
