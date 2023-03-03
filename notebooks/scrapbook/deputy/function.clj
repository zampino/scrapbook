(ns scrapbook.deputy.function
  (:require
   [deputy.core :as d]
   [deputy.syntax :as s :refer :all]
   [deputy.extensions.lets]
   [scrapbook.deputy.macros :refer [define]]))

(deputy.ast/unparse (deputy.syntax/parse ident))

(d/defterm [id [T :type] (=> T T)] (fun [x] x))
(d/defterm [∘ [A :type] [B :type] [C :type]
            [g (=> B C)] [f (=> A B)] (=> A C)]
  (fun [a] (g (f a))))
