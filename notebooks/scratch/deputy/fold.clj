;; Fold scratch
(ns scratch.deputy.fold
 (:require
  [clojure.string :as str]
  [deputy.ast :as a]
  [deputy.core :as d :refer [defterm try-defterm]]
  [deputy.extensions.labels]
  [deputy.extensions.desc]
  [deputy.extensions.fix :as r]
  [deputy.norm :as n]
  [deputy.stdlib.defdata :refer [defdata]]
  [deputy.stdlib.bottom :as bot]
  [deputy.stdlib.nat :refer [Nat ze su add one two three]]
  [deputy.stdlib.list :as list :refer [List List-case lnil lcons]]
  [deputy.syntax :as s :refer :all]
  [scrapbook.deputy.equality :as eq :refer [l= ≗]]
  [nextjournal.clerk :as clerk]
  [deputy.typing :as t]
  [deputy.unparse :as up]
  [deputy.utils :as u :refer [ko-expr? throw-ko ok> example examples]]
  ))

(defmacro compute [term] `(a/unparse (n/evaluate (s/parse ~term))))

;; ### Recursion Semantic
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

;; fold : {A,B : :type} -> (f : (-> A B B)) -> B -> List A -> B
;; fold f b [] = b
;; fold f b a::as = f a (fold f b as)

;; fold definition in terms of rec
#_
(defterm [foldr
            [A :type]
            [B :type]
            [f (=> A B B)]
            [b B]
            [l (List A)]
            B]
  (r/fix (List A) (λ [_] B) l
         (λ [l]
            (List-case A l
                       (λ [_] (r/rec (List A) (λ [_] B) B))
                       (r/ret b)
                       (λ [a as] (r/rec-call as (λ [y] (r/ret (f a y)))))
                       ))))


(t/type-check Nat one)
(t/type-check (s/parse (List Nat)) (s/parse (lcons Nat one (lnil Nat))))
(a/unparse (t/type-synth (s/parse (lnil Nat))))
(a/unparse
 (t/type-synth (s/parse (lcons Nat one (lnil Nat)))))

(defterm [l1 (List Nat)]
  (lcons Nat one (lnil Nat)))

(defterm [l2 (List Nat)]
  (lcons Nat two l1))

(defterm [l3 (List Nat)]
  (lcons Nat three l2))

#_
(time
 (compute (fold Nat Nat add ze l3)))


;; We define the right fold by induction on lists

(defterm [foldr
          [A :type]
          [B :type]
          [f (=> A B B)]
          [b B]
          [l (List A)]
          B]
  (list/ind A l (λ [_] B) b (λ [a l b] (f a b))))

(defterm [foldr-U2 [A :type] [B :type] [f (=> A B B)] [b B]
          (∀ [x A]
             (∀ [xs (List A)]
                (l= B
                    (f x (foldr A B f b xs))
                    (foldr A B f b (lcons A x xs)))))]
  (fun [x xs] (eq/l=-refl B (foldr A B f b (lcons A x xs)) #_(f x (foldr A B f b xs)))))

#_
(time
 (compute (foldr Nat Nat add ze l3)))

;; Fold universality
(s/defparse ∀ ::s/pi)

;; repro issue
(defterm [repro [A :type] [B :type]
          [h (=> (List A) B)]
          [b B] [f (=> A B B)]
          [a A]
          (∀ [as (List A)]
             (=> (l= B (h as) (foldr A B f b as))
                 (l= B
                     (f a (h as))
                     (f a (foldr A B f b as)))))]
  (fun [l e]
       (eq/l=-cong B B (f a)
                   (h l)
                   (foldr A B f b l)
                   e)))

;; universality of fold

(defterm [foldr-universal [A :type] [B :type]
              [h (=> (List A) B)]
              [b B] [f (=> A B B)]
              [u1 (l= B (h (lnil A)) b)]
              [u2 (∀ [a A]
                     (∀ [as (List A)]
                        (l= B
                            (h (lcons A a as))
                            (f a (h as)))))]
              (≗ (List A) B h (foldr A B f b))]
  (fun [xs]
       (list/ind A xs
                 (fun [l] (l= B (h l) (foldr A B f b l)))
                 u1
                 (fun [a as Pas]
                      (eq/l=-trans B
                                   (h (lcons A a as))
                                   (f a (h as))
                                   (f a (foldr A B f b as))
                                   (u2 a as)
                                   (eq/l=-cong B B
                                               (f a)
                                               (h as)
                                               (foldr A B f b as)
                                               Pas))))))

;;

;; fusion
;; https://github.com/agda/agda-stdlib/blob/master/src/Data/List/Properties.agda
;; needs ptwise-eq

(comment
  (doseq [[n _] (ns-publics *ns*)] (ns-unmap *ns* n)))

(defterm [list-length [A :type] [l (List A)] Nat]
  (list/ind A l (λ [_] Nat) ze
            (λ [a l n] (su n))))

#_#_
(compute (list-length Nat l1))
(compute (list-length Nat l3))
