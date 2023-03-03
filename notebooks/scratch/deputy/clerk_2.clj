;; # Deputy and Clerk
(ns scratch.deputy.clerk-2
  {:nextjournal.clerk/no-cache true}
  (:require
   [deputy.ast :as a]
   [deputy.core :as d :refer [defterm try-defterm]]
   [deputy.norm :as n]
   [deputy.stdlib.nat :refer [Nat ze su add one two three]]
   [deputy.stdlib.list :as list :refer [List List-case lnil lcons]]
   [deputy.syntax :as s :refer :all]
   [nextjournal.clerk :as clerk]
   [nextjournal.clerk.viewer :as clerk.viewer]
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

(try-defterm [foldr [A :type][B :type]
          [f (=> A B B)][b B]
          [l (List A)] B]
  (list/ind A ?L (λ [_] B) b (λ [a _ acc] (f a acc))))

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

(comment
  (clerk/clear-cache!)
  (clerk.viewer/reset-viewers! clerk.viewer/default-viewers))

#_
(clerk.viewer/add-viewers!
 [{:pred (every-pred map? :def-id :vtype)
   :transform-fn (comp clerk/mark-presented
                       #(assoc % :nextjournal/width :wide)
                       (clerk/update-val #(a/unparse (n/evaluate (:vtype %)))))
   :render-fn '(fn [vtype _]
                 [:div.bg-green-200.p-5
                  [:div [:span.underline.font-sans.text-bold.text-green-600 "Checked!"]]
                  [:div.text-lg
                   [v/inspect vtype]]])}])
