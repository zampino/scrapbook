(ns scratch.flow
  (:require [missionary.core :as m]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))

(def !a (atom  0))

(comment

  (def >out (m/latest inc (m/watch !a)))

  (def out (>out #(prn :notify)
                 #(prn :terminate)))

  (swap! !a inc)


  (doto (Thread. (fn [] (prn :consume @out))) .start)
  )
