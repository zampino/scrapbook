(ns scratch.deputy.sets
  (:refer-clojure :exclude [set])
  (:require
   [deputy.core :as d]
   [deputy.syntax :as s :refer :all]
   [deputy.ast :as a]
   [deputy.unparse :as u]
   [deputy.stdlib.defdata :refer [defdata]]
   [deputy.stdlib.bottom :as b]
   [deputy.norm :as n]
   [deputy.typing :as t]
   [scratch.deputy.equality :as eq]
   [nextjournal.clerk :as clerk]
   [nextjournal.clerk.viewer :as v]))


;; how do I define this?
#_ (def ⊥ b/bottom)

(d/defterm [¬ [T :type] :type] (=> T b/bottom))
(s/defparse ∃ ::s/sigma)
(s/defparse ∀ ::s/pi)

(d/defterm [set [T :type] :type] (=> T :type))

(d/defterm [∈ [T :type] [x T] [s (set T)] :type] (s x))

(d/defterm [set-equal [T :type] [s1 (set T)] [s2 (set T)] :type]
  (eq/l= (set T) s1 s2))
