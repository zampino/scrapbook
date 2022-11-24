(ns scratch.deputy.cantor
  (:refer-clojure :exclude [set])
  (:require
   [deputy.core :as d]
   [deputy.syntax :as s :refer :all]
   [deputy.ast :as a]
   [deputy.unparse :as u]
   [deputy.stdlib.defdata :refer [defdata]]
   [deputy.norm :as n]
   [deputy.typing :as t]
   [nextjournal.clerk :as clerk]
   [nextjournal.clerk.viewer :as v]))


(d/defterm [set [A :type] :type] (=> A :type))
