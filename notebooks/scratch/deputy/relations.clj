(ns scratch.depouty.relations
 (:require
  [clojure.string :as str]
  [deputy.ast :as a]
  [deputy.core :as d]
  [deputy.extensions.fix :as r]
  [deputy.norm :as n]
  [deputy.stdlib.defdata :refer [defdata]]
  [deputy.stdlib.nat :refer [Nat ze su add]]
  [deputy.syntax :as s :refer :all]
  [nextjournal.clerk :as clerk]
  [deputy.typing :as t]
  [deputy.unparse :as up]
  [deputy.utils :as u :refer [ko-expr? throw-ko ok> example examples]]
  ))


(defdata Eq [A :type] (refl [x A] [y A]))

(defdata Leq []
  (zleqn [n Nat] )
  (sleqs [n Nat] [m Nat]))
