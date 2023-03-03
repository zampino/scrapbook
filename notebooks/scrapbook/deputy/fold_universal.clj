;; # ⏛⏛ Fold Fusion ⏛⏛⏛⏛
;; _**[DRAFT]** Whaat? Dependent types in Clojure
;; with inductive datatypes. We can finally __prove__ in Clojure why
;; Clojure transducers actually fuse well._
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
  [scrapbook.deputy.equality :as eq :refer [≡ ≗]]
  [scrapbook.deputy.function :refer [∘ id]]))

(defmacro compute [term] `(a/unparse (n/evaluate (s/parse ~term))))
(s/defparse ∀ ::s/pi)
(s/defparse ∃ ::s/sigma)

;; Our friends at Agda have some structural-recursion sugar in their definitions:
;;
;;    foldr : {A,B : :type} -> (f : (-> A B B)) -> B -> List A -> B
;;    foldr f b [] = b
;;    foldr f b a::as = f a (fold f b as)
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
(clerk.eval/time-ms
 (compute (foldr Nat Nat add ze l3)))

;; ## Fold Universality

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
(defterm [foldr-universal [A :type] [B :type][f (=> A B B)][b B]
          [h (=> (List A) B)]
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
                                             Pas))))))

;; **NOTE:** here deputy made some troubles trying to improve readability via some lets, but inlining everything is also ok
;; (type synthesis complains about unsupported terms)

;; Fold fusion proof à la Agda
;; https://agda.github.io/agda-stdlib/Data.List.Properties.html#16046

;; _Given f and g left actions of A on B and C respectively and a
;; a morphism m of f to g, then we can fuse m with fold_
(defterm [fold-fusion
          [A :type] [B :type] [C :type]
          [f (=> A B B)] [g (=> A C C)]
          [m (=> B C)]
          [m-morph (∀ [a A]
                      (∀ [b B]
                         (≡ C (m (f a b)) (g a (m b)))))]
          [b B]
          (≗ (List A) C
             (∘ (List A) B C m (foldr A B f b))
             (foldr A C g (m b)))]
  (foldr-universal A C g (m b)
                   (∘ (List A) B C m (foldr A B f b))
                   (eq/≡-refl C (m b))
                   (fun [a as] (m-morph a (foldr A B f b as)))))

;; map

(defterm [map [A :type][B :type]
          [f (=> A B)] [as (List A)] (List B)]
  (list/ind A as (λ [_] (List B)) (lnil B) (λ [a as fas] (lcons B (f a) fas))))

;; surprise: map is fold nauturally
(defterm [map-is-fold [A :type] [B :type] [f (=> A B)]
          (≗ (List A) (List B)
             (map A B f)
             (foldr A (List B) (fun [a bs] (lcons B (f a) bs)) (lnil B)))]
  (foldr-universal
   A (List B) (fun [a bs] (lcons B (f a) bs)) (lnil B)
   (map A B f)
   (eq/≡-refl (List B) (lnil B))
   (fun [a as] (eq/≡-refl (List B) (map A B f (lcons A a as))))))

;; left actions
(defterm [Act [A :type] [B :type] :type] (=> A B B))
(defterm [End [X :type] :type] (=> X X))

#_
(defterm [map-xf [A :type] [B :type] [f (=> A B)] (End (Act A (List B)))]
  (fun [r]
       (fun [a bs] (lcons B (f a) bs))))

#_
(defterm [cons-xf [T :type] (Act T (List T))] (lcons T))


#_
(defterm [transducer-lemma [A :type] [B :type] [xf (End (Act A (List B)))] [bs (List B)]
          [r (Act A (List B))]

          (≗ (List B) (List B)
             (∘ (List A) (List B) (List B)
                (fold A (List B) bs r)
                (fold B (List B) (xf (lcons B))))
             (fold A (List B) (xf r)))])


#_(defterm [map-transducer-lemma [A :type] [B :type] [f (=> A B)] [bs (List B)]
            [r (Act A (List B))]
            (≗ (List B) (List B)
               (∘ (List A) (List B) (List B)
                  (fold A (List B) bs r)
                  (map A B f))
               (fold A (List B) (map-xf r)))])

;; transducer lemma
;; (map-xf f) : r |→ λ a bs (r (f a) bs)
;;  r ∈ (Act A (List B))
;; fold g ∘ map f ≗ fold ((map-xf f) r)

;; fold (g r) is morphism from ((∘ g f) r) to (f r)
;;(∀ [a A]
;;   (∀ [bs (List B)]
;;      (≡ (List B) ((fold (g r)) (((∘ g f) r) a bs))
;;
;;                   ((f r) a ((fold (g r)) bs)))))
;;


(comment
  (clerk/clear-cache!)
  (ex-data *e))

;; **TODO**: show map and filter are folds
;; **TODO**: show clojure transducers as an example of fold
;;
;; **TODO**: show left fold from right-fold, can we define `foldl` directly in `(rec A B X)` semantics? Does tail-recursiveness fit into that pattern?
;;
;; ## References
;; [Hutton] https://www.cs.nott.ac.uk/~pszgmh/fold.pdf
#_
(defterm [transducer-lemma [A :type] [B :type]
          [f (End (Act A (List B)))] [g (End (Act A (List B)))]
          [r (Act A (List B))]
          ])
