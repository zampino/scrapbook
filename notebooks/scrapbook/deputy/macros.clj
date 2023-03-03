(ns scrapbook.deputy.macros
  (:require [deputy.core :as d]))

(defmacro define [name params body] `(d/defterm [~name ~@params] ~body))

(comment

  (macroexpand-1 '(define ≡ [[T :type] [x T] [y T] :type]
                  (Π [P (=> T :type)] (=> (P x) (P y))))))
