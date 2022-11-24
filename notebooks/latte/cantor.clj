;; # A proof of Cantor Theorem in Clojure
(ns latte.cantor
  (:refer-clojure :exclude [and or not set])
  (:require [latte.core :refer [defthm defaxiom definition example
                                lambda try-proof proof assume have pose qed
                                type-of type-check?]]
            [latte-prelude.quant :refer [ex exists ex-elim] :as q]
            [latte-prelude.prop :refer [and or not absurd] :as prop]
            [latte-prelude.equal :refer [equal] :as e]
            [latte-sets.set :refer [set set-of elem set-equal set-equal-prop]
             :as sets]))

(example
 [[T :type] [P (==> T :type)]]
 (set T)
 (qed (set-of [x T] (P x))))

(definition section
  "A section of T is a map from T to sets over T"
  [[T :type]]
  (==> T (set T)))

(definition section-surjective
  "a set-based definition for surjectivity"
  [[T :type] [m (section T)]]
  (forall [s (set T)]
          (exists [x T] (set-equal (m x) s))))

(definition ad
  "Given a type T and a section m of T, the antidiagonal of m is the set of inhabitants of T which avoid the diagonal of m"
  [[T :type] [m (section T)]]
  (lambda [x T] (not (elem x (m x)))))

(defthm cantor-theorem
  "Given a type T, there is no surjective section of T."
  [[T :type] [m (section T)]]
  (not (section-surjective T m)))

(type-check?
 [T :type][m (section T)]
 (ad T m)
 (set T))


(proof 'cantor-theorem
  "Take the antidiagonal of T via m"
  (pose AntiDiag := (ad T m))

  "For convenience, we give a name to the pre-image of the anti-diagonal via m"
  (pose AntiDiagPreimage := (lambda [x T] (set-equal (m x) AntiDiag)))

  (assume [Hs (section-surjective T m)]

    "By the hypothesis above, there exists a pre-image of the anti-diagonal, store its proof for later"
    (have adp-exists (ex AntiDiagPreimage) :by (Hs AntiDiag))

    "we'll prove that for all t, if t whitnesses the anti-diagonal pre-image, then we have absurdity"
    (assume [t T]
      (pose N := (lambda [s (set T)] (not (elem t s))))

      "assume the pre-image of the antidiagonal is inhabited _and_ so is the antidiagonal"
      (assume [p (AntiDiagPreimage t)
               e (AntiDiag t)]
        (have <1> (not (elem t (m t))) :by e)

        (have <2> (not (AntiDiag t)) :by ((set-equal-prop (m t) AntiDiag N)
                                          p <1>))


        (have <abs-l> absurd :by ((prop/absurd-intro (AntiDiag t)) e <2>)))

      "assume there is a pre-image of the antidiagonal _and_ take an element which avoids the antidiagonal"
      (assume [p (AntiDiagPreimage t)
               e (not (AntiDiag t))]

        (have <3> (not (elem t AntiDiag)) :by e)
        (have <4> (set-equal AntiDiag (m t))
              :by ((sets/set-equal-sym (m t) AntiDiag) p))

        (have <5> (not (elem t (m t))) :by ((set-equal-prop AntiDiag (m t) N) <4> <3>))
        (have <6> (AntiDiag t) :by <5>)

        (have <abs-r> absurd :by ((prop/absurd-intro (AntiDiag t)) <6> e)))

      "Recall that (not A) is (==> A absurd), hence by discharging `p` and `e` we have"

      (have <left>  (==> (AntiDiagPreimage t) (not (AntiDiag t))) :by <abs-l>)
      (have <right> (==> (AntiDiagPreimage t) (not (not (AntiDiag t)))) :by <abs-r>)

      "by non-contradiction, we have"
      (have <a> (==> (AntiDiagPreimage t) absurd)
            :by (lambda [x (AntiDiagPreimage t)] ((<right> x) (<left> x)))))

    "discharge t"
    (have <b> (forall [t T] (==> (AntiDiagPreimage t) absurd)) :by <a>)
    (have ⚡ absurd :by (ex-elim adp-exists <b>)))

   (qed ⚡))
