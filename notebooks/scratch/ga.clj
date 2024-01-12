(ns scratch.ga
  {:nextjournal.clerk/eval :sci
   :nextjournal.clerk/render-evaluator :cherry}
  (:require [applied-science.js-interop :as j]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk]))
;; [ts-geometric-algebra](https://github.com/frostburn/ts-geometric-algebra/tree/main)

(def ℂ (ga/Algebra 0 1))

(array 0 1)

(def one (.fromGanja ℂ (array 1 0)))
(def i (.fromGanja ℂ (array 0 1)))

(.mul i i )
(.mul one one)


(def a (.fromGanja ℂ (array 3 2)))
(def b (.fromGanja ℂ (array 1 4)))

(.mul a b)

(def PG2 (ga/Algebra 3 0 1))
(js/console.log :PG PG2)
