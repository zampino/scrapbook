(ns scratch.macros)

(defmacro dep-def [name & deps]
  (assert (every? #(some-> % resolve bound?) deps)
          "All deps must to be defined and bound at macro-expansion!")
  `(def ~name {:name '~name :refs '~deps}))

(comment
  (dep-def X)
  (dep-def Z)
  (dep-def Y X Z))
