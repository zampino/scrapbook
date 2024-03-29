;; # ⋰⋰⋰ Cantor Theorem ⋰⋰⋰
;; _This short article shows some features from [Deputy](https://gitlab.com/fredokun/deputy), a dependentyly-typed language
;; embedded in clojure by Frederic Peschanski and Pierre-Evariste Dagand._

(ns scrapbook.deputy.cantor
  (:refer-clojure :exclude [set])
  (:require
   [deputy.core :as d :refer [defterm]]
   [deputy.syntax :as s :refer :all]
   ;; labels need to be required before bottom, throws otherwise
   [deputy.extensions.labels]
   [deputy.stdlib.bottom :refer [bottom]]
   [scrapbook.deputy.equality :refer [≡] :as eq]
   [nextjournal.clerk :as clerk]))

;; My favourite hello-world proof for dependently typed languages is [Cantor theorem](https://en.wikipedia.org/wiki/Cantor's_theorem).
;; The proof of the theorem is short but touches:
;; * Universal and Existential quantification
;; * Leibniz equality or _the indiscernibility of equals_ (defined in `scrapbook.deputy.equality`)
;; * The _principle of non contradiction_ (no need for the excluded middle)
;; * The ambiguity between types and sets,
;; * Sets as predicates
;;
;; We'll be using the principle of non-cotradiction
;; hence we need to define the negation of a proposition.

#_ (defdata ⊥ "The empty data type" [])

(defterm [¬ [T :type] :type] (=> T bottom))
(defterm [⊥-intro [T :type] [t T] [¬t (¬ T)] bottom] (¬t t))

;; Deputy allows for introducing new syntax (_let this parse as that_), nice!
{::clerk/visibility {:result :hide}}
(s/defparse ∃ ::s/sigma)
(s/defparse ∀ ::s/pi)
{::clerk/visibility {:result :show}}

;; The usual _sets as predicates_ definition
(defterm [set [T :type] :type] (=> T :type))
;; and set-membership as a binary relation
(defterm [∈ [T :type] [x T] [s (set T)] :type] (s x))

;; A surjective map, existence of preimages
(defterm [surjective [A :type] [B :type] [f (=> A B)] :type]
  (∀ [b B] (∃ [a A] (≡ B (f a) b))))

;; And here the celeberrimus diagonal-avoiding beautiful trick anno 1891.
(defterm [anti-diagonal [T :type] [f (=> T (set T))] (set T)]
  (fun [x] (¬ (∈ T x (f x)))))

^{::clerk/visibility {:code :hide}}
(clerk/html
  [:div.text-xl
   [:span.font-sans.font-bold "Cantor Theorem:"]
   [:em.ml-1.font-sans "There is no surjective function from a set to its powerset."]])

(defterm [cantor-theorem [T :type] [f (=> T (set T))] (¬ (surjective T (set T) f))]
  (fun [fsu]
       ;; assume we have a surjective f from T to (set T)
       (lets [[adf (set T)] (anti-diagonal T f)
              [ex-preimg (∃ [t T] (≡ (set T) (f t) adf))] (fsu adf)
              ;; let t witness existence of a preimage
              [t T] (π1 ex-preimg)
              [ft=adf (≡ (set T) (f t) adf)] (π2 ex-preimg)
              ;; define a predicate P which describes sets not containing t
              [P (=> (set T) :type)] (fun [s] (¬ (∈ T t s)))

              ;; t cannot belong to the anti-diagonal of f
              [¬t∈adf (¬ (∈ T t adf))] (fun [t∈adf] (⊥-intro (∈ T t adf) t∈adf (ft=adf P t∈adf)))
              [adf=ft (≡ (set T) adf (f t))] (eq/≡-sym (set T) (f t) adf ft=adf)
              ;; t cannot not belong to the anti-diagonal of f
              [¬¬t∈adf (¬ (¬ (∈ T t adf)))] (fun [¬t∈adf] (¬t∈adf (adf=ft P ¬t∈adf)))]
         (⊥-intro (¬ (∈ T t adf)) ¬t∈adf ¬¬t∈adf))))


#_
(comment
  (clerk/clear-cache!)
  (ex-data *e))
