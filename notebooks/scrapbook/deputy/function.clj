(ns scrapbook.deputy.function
  (:require
   [deputy.core :as d]
   [deputy.syntax :as s :refer :all]
   [deputy.extensions.lets]))

(d/defterm [id [T :type] (=> T T)] (fun [x] x))
(d/defterm [∘ [A :type] [B :type] [C :type]
            [g (=> B C)] [f (=> A B)] (=> A C)]
  (fun [a] (g (f a))))
