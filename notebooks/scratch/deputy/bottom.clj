(ns scratch.deputy.bottom
  (:refer-clojure :exclude [set])
  (:require
   [deputy.core :as d]
   [deputy.syntax :as s :refer :all]
   [deputy.stdlib.bottom :as b]
   [scratch.deputy.equality :as eq]))

(d/defterm [¬ [T :type] :type] (=> T b/bottom))
(d/defterm [⊥-intro [T :type] [t T] [¬t (¬ T)] b/bottom] (¬t t))
