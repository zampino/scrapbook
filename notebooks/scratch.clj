(ns scratch
  (:require [scratch.macros :refer [dep-def]]))

(dep-def X)
(dep-def Y X)


(comment

  (nextjournal.clerk/show! 'scratch))
