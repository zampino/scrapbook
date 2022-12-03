;; # 🪐 The Universality of Fold
;; _Proven in Clojure with Deputy Dependent Types_
(ns scrapbook.deputy.fold-universal
 (:require
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
  [nextjournal.clerk :as clerk]
  [nextjournal.clerk.eval :as clerk.eval]
  [scrapbook.deputy.equality :as eq :refer [l= ≗]]))

(defmacro compute [term] `(a/unparse (n/evaluate (s/parse ~term))))
(s/defparse ∀ ::s/pi)

;; Our friends at Agda have some structural-recursion sugar in function definition:

;;    fold : {A,B : :type} -> (f : (-> A B B)) -> B -> List A -> B
;;    fold f b [] = b
;;    fold f b a::as = f a (fold f b as)

;; while Deputy inductive types come with an induction/recursion helper, we define the right fold by induction on lists
(defterm [foldr [A :type][B :type]
          [f (=> A B B)][b B]
          [l (List A)] B]
  (list/ind A l (λ [_] B) b (λ [a l b] (f a b))))

{::clerk/visibility {:result :hide}}
(defterm [l1 (List Nat)] (lcons Nat one (lnil Nat)))
(defterm [l2 (List Nat)] (lcons Nat two l1))
(defterm [l3 (List Nat)] (lcons Nat three l2))
{::clerk/visibility {:result :show}}

;; computing a fold might take some time :-)
;; ^::clerk/no-cache
(clerk.eval/time-ms
 (compute (foldr Nat Nat add ze l3)))

;; fold properties are computationally satisfied!
(defterm [foldr-U1 [A :type] [B :type] [f (=> A B B)] [b B]
          (l= B (foldr A B f b (lnil A)) b)]
   (eq/l=-refl B b))

(defterm [foldr-U2 [A :type] [B :type] [f (=> A B B)] [b B]
          (∀ [x A]
             (∀ [xs (List A)]
                (l= B
                    (f x (foldr A B f b xs))
                    (foldr A B f b (lcons A x xs)))))]
  (fun [x xs] (eq/l=-refl B (foldr A B f b (lcons A x xs)))))

;; ### Fold universality

;; As function of lists, fold is uniquely determined by properties
;;
;; $$
;; \def\fold{\mathsf{fold}}
;; \begin{align}
;; \textbf{(u1)}&\quad\quad &(\fold f\, b\, []) &= b \\
;; \textbf{(u2)}&\quad\quad &(\fold f\, b\, a::as) &= (f a\, (\fold f\, b\, as))
;; \end{align}
;; $$

(clerk.eval/time-ms
 (defterm [foldr-universal [A :type] [B :type]
           [h (=> (List A) B)]
           [b B] [f (=> A B B)]
           [u1 (l= B (h (lnil A)) b)]
           [u2 (∀ [a A]
                  (∀ [as (List A)]
                     (l= B
                         (h (lcons A a as))
                         (f a (h as)))))]
           ;; pointwise equality
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
                                    (eq/l=-cong B B (f a)
                                                (h as)
                                                (foldr A B f b as)
                                                Pas)))))))

;; **NOTE:** here deputy made some troubles trying to improve readability via some lets, but inlining everything is also ok
;; (type synthesis complains about unsupported terms)

;; **TODO:** fusion
;; https://github.com/agda/agda-stdlib/blob/master/src/Data/List/Properties.agda
