;; # 🪐 The Universality of Fold
;; _As Clojurists we all love transducers, and we "believe" they shine at
;; delivering elegant, modular and efficient code when chained.
;; Their dynamics is explained by the fusion property of fold, this can be
;; actually **proven** by means of Deputy's Dependent Types_.
(ns scrapbook.deputy.fold-universal
 (:require
  [deputy.ast :as a]
  [deputy.core :as d :refer [defterm try-defterm]]
  [deputy.norm :as n]
  [deputy.stdlib.nat :refer [Nat ze su add one two three]]
  [deputy.stdlib.list :as list :refer [List List-case lnil lcons]]
  [deputy.syntax :as s :refer :all]
  [nextjournal.clerk :as clerk]
  [nextjournal.clerk.eval :as clerk.eval]
  [scrapbook.deputy.equality :as eq :refer [≡ ≗]]))

(defmacro compute [term] `(a/unparse (n/evaluate (s/parse ~term))))
(s/defparse ∀ ::s/pi)

;; Our friends at Agda have some structural-recursion sugar in their definitions:
;;
;;    fold : {A,B : :type} -> (f : (-> A B B)) -> B -> List A -> B
;;    fold f b [] = b
;;    fold f b a::as = f a (fold f b as)
;;
;; while Deputy inductive lists come equipped with an induction/recursion (? terminology) helper by means of which we can define the right fold as
(defterm [foldr [A :type][B :type]
          [f (=> A B B)][b B]
          [l (List A)] B]
  (list/ind A l (λ [_] B) b (λ [a _ acc] (f a acc))))

{::clerk/visibility {:result :hide}}
(defterm [l1 (List Nat)] (lcons Nat one (lnil Nat)))
(defterm [l2 (List Nat)] (lcons Nat two l1))
(defterm [l3 (List Nat)] (lcons Nat three l2))
{::clerk/visibility {:result :show}}

;; computing a fold might take some time :-)
;; ^::clerk/no-cache
(clerk.eval/time-ms
 (compute (foldr Nat Nat add ze l3)))

;; ### Fold Universality

;; As function of lists, fold is uniquely determined by properties
;;
;; $$
;; \def\fold{\mathsf{fold}}
;; \begin{align}
;; (\fold f\, b\, []) &= b &\quad\quad\textsf{(U1)}\\
;; (\fold f\, b\, a::as) &= (f a\, (\fold f\, b\, as))&\quad\quad\textsf{(U2)}
;; \end{align}
;; $$
;; Fold properties are computationally satisfied by our definition above!
(defterm [foldr-U1 [A :type] [B :type] [f (=> A B B)] [b B]
          (≡ B (foldr A B f b (lnil A)) b)]
  (eq/≡-refl B b))

(defterm [foldr-U2 [A :type] [B :type] [f (=> A B B)] [b B]
          (∀ [x A]
             (∀ [xs (List A)]
                (≡ B
                    (f x (foldr A B f b xs))
                    (foldr A B f b (lcons A x xs)))))]
  (fun [x xs] (eq/≡-refl B (foldr A B f b (lcons A x xs)))))


;; Now given a left-action $f$ of $A$ on $B$, any arbitrary function of lists satisfying $\textsf{(U1)}$ and $\textsf{(U2)}$ above is actually a fold!
(clerk.eval/time-ms
 (defterm [foldr-universal [A :type] [B :type]
           [h (=> (List A) B)]
           [b B] [f (=> A B B)]
           [u1 (≡ B (h (lnil A)) b)]
           [u2 (∀ [a A]
                  (∀ [as (List A)]
                     (≡ B
                        (h (lcons A a as))
                        (f a (h as)))))]
           ;; pointwise equality
           (≗ (List A) B h (foldr A B f b))]
   (fun [xs]
        (list/ind A xs
                  (fun [l] (≡ B (h l) (foldr A B f b l)))
                  u1
                  (fun [a as Pas]
                       (eq/≡-trans B
                                   (h (lcons A a as))
                                   (f a (h as))
                                   (f a (foldr A B f b as))
                                   (u2 a as)
                                   (eq/≡-cong B B (f a)
                                              (h as)
                                              (foldr A B f b as)
                                              Pas)))))))

;; **NOTE:** here deputy made some troubles trying to improve readability via some lets, but inlining everything is also ok
;; (type synthesis complains about unsupported terms)

;; **TODO:** fold fusion
;; https://agda.github.io/agda-stdlib/Data.List.Properties.html#16046
;; **TODO**: show left fold
;; ## References
;; [Hutton] https://www.cs.nott.ac.uk/~pszgmh/fold.pdf
(comment
  (clerk/clear-cache!)
  (ex-data *e))
