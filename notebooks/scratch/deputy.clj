(ns scratch.deputy
  (:require
   [clojure.string :as str]
   [deputy.ast :as a]
   [deputy.core :as d]
   [deputy.extensions.fix :as r]
   [deputy.norm :as n]
   [deputy.stdlib.defdata :refer [defdata]]
   [deputy.stdlib.nat :refer [Nat ze su add]]
   [deputy.syntax :as s :refer :all]
   [nextjournal.clerk :as clerk]
   [deputy.typing :as t]
   [deputy.unparse :as up]
   [deputy.utils :as u :refer [ko-expr? throw-ko ok> example examples]]
   ))

(d/defterm [n1 Nat] (su ze))
(d/defterm [n2 Nat] (su (su ze)))
(n/evaluate (s/app su ze))
(s/parse (add ze n1))
(n/evaluate (s/parse (add ze n1 )))
(s/parse (su ze))

(defmacro β-eq? [t1 t2] `(n/beta-equiv (s/parse ~t1) (s/parse ~t2)))

(n/beta-equiv
  (s/parse (add ze n1))
  (s/parse (add n1 ze)))

(β-eq?
 (add ze n1)
 (add n1 ze))

#_
(β-eq?
 (add (su ze) n1)
 two)



#_(doseq [[n _] (ns-publics *ns*)] (ns-unmap *ns* n))

;; redefine nat
(defdata nat
   "The inductive type of natural numbers." []
   (z)
   (suc [n <rec>]))
(ex-data *e)
(deref d/last-error)

(d/defterm [one nat] (suc z))
(d/defterm [two nat] (suc (suc z)))
(d/defterm [three nat] (suc (suc (suc z))))


(β-eq? two (suc one))
(β-eq? three (suc (suc one)))
(β-eq? three (suc two))

;; unparse ~= evaluates in clojure
(n/evaluate (s/parse (su (su ze))))
(a/unparse (n/evaluate (s/parse (su (su ze)))))

;; + : (n : nat) -> (m : nat) -> nat
;; + 0 m = m
;; + (suc n) m = (suc (+ n m))

;; Recursion Semantic
;; [A :type][B (=> A :type)][X :type] ⊢
;; :type ∋ rec A B X
;;
;; [A :type][B (=> A :type)][X :type][v X] ⊢
;; rec A B X ∋ ret v
;;
;; [A :type][B (=> A :type)][X :type][a A][k (=> (B a) (rec A B X))] ⊢
;; rec A B X ∋ rec-call a k
;;
;; [A :type][B (=> A :type)][X :type][a (rec A B X)][r (Π [a A] (B a))] ⊢
;; (red A B X a r) ∈ X
;;
;; [A :type][B (=> A :type)][a A][r (Π [a A] (rec A B (B a)))] ⊢
;; (fix A B a r) ∈ (B a)

;; plus : N -> N -> N
;; plus 0 m = m
;; plus (suc n) m = suc (plus n m)

(d/defterm [plus  [n nat] [m nat] nat]
  (r/fix nat (λ [_] nat) n
         ;; r : (a : A) -> (rec A B (B a))
         ;; B is (λ [_] nat)
         (fun [n]
              (nat-case
               n
               (λ [n] (r/rec nat (λ [_] nat) nat))
               (r/ret m)
               (fun [x]
                    (r/rec-call x (fun [y] (r/ret (suc y)))))))))




(β-eq? (plus z one ) one)
(β-eq? (plus one z ) one)
(β-eq? (plus one one) two)
(β-eq? three (plus two one))

;; https://www.cse.chalmers.se/~peterd/papers/DependentTypesAtWork.pdf
;; nat-rec : {C : :type} -> C -> (nat -> C -> C) -> nat -> C
;; nat-rec p h zero = p
;; nat-rec p h (suc n) = h n (nat-rec p h n)

;; fold : {A : :type} ->  (A -> B -> B) -> B -> [A] -> B
;; fold f b [] = b
;; fold f b a::as = f a (fold f b as)

;; ind : (n : nat) -> (P : nat -> :type) -> (P 0) -> (ih : ∀ (i : nat) -> (P i) -> (P (suc i))) -> (P n)

;; nat-rec => ind
;; P : n -> :type
;; ih
;; p0 : (P 0)
;; C = Π n : (P n)
;; ind n P p0 ih = nat-rec {C = } p0 (λ [n c] (ih n c)) n



;; Recursion Semantic
;; [A :type][B (=> A :type)][X :type] ⊢
;; :type ∋ rec A B X
;;
;; [A :type][B (=> A :type)][X :type][v X] ⊢
;; rec A B X ∋ ret v
;;
;; [A :type][B (=> A :type)][X :type][a A][k (=> (B a) (rec A B X))] ⊢
;; rec A B X ∋ rec-call a k
;;
;; [A :type][B (=> A :type)][X :type][a (rec A B X)][r (Π [a A] (B a))] ⊢
;; (red A B X a r) ∈ X
;;
;; [A :type][B (=> A :type)][a A][r (Π [a A] (rec A B (B a)))] ⊢
;; (fix A B a r) ∈ (B a)

(d/defterm [nat-rec [C :type]
            [c C]
            [h (=> nat C C)]
            [n nat]
            C]
  (r/fix nat (λ [_] C) n
         (fun [n]
              (nat-case
               n
               (λ [n] (r/rec nat (λ [_] C) C))
               (r/ret c)
               (fun [x] (r/rec-call x (fun [c] (r/ret (h x c)))))))))

(d/defterm [rplus [n nat] [m nat] nat]
  (nat-rec nat m (λ [x c] (suc c)) n))


(β-eq? (rplus z one ) one)
(β-eq? (rplus one z ) one)
(β-eq? (rplus one one) two)
(β-eq? three (rplus two one))


(comment
  (keep (fn [[k v]] (when (var? v)
                      (when (some-> v symbol namespace (str/starts-with? "deputy"))
                        k)))
          (ns-map *ns*))

  (ffirst (ns-map *ns*))
  (ex-data *e)
  (doseq [[n _] (ns-publics *ns*)] (ns-unmap *ns* n)))



;; typing glitch?

(d/defterm [set [A :type] :type] (=> A :type))
(d/defterm [powerset [A :type] :type] (=> (set A) :type))

(d/defterm [A :type] :unit)

;; ???
(= (:vtype set)
   (:vtype powerset))

(a/unparse (:vtype set))
(a/unparse (:vtype powerset))

;; trying to define a powerset
(t/type-synth
 (s/parse (=> (set A) :type)))
