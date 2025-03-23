(ns frp
  {:nextjournal.clerk/no-cache true
   :nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [nextjournal.clerk :as clerk])
  (:import (java.util.concurrent TimeUnit ArrayBlockingQueue)))

(defonce inbox
  (new ArrayBlockingQueue 100))

(defonce outbox
  (new ArrayBlockingQueue 100))

(defn poll []
  (or (.poll outbox 3 TimeUnit/SECONDS)
      (throw (ex-info "Pending" {}))))

(defn )

(defn random-pill []
  (let [color (first (shuffle ["red" "indigo" "sky" "amber"]))
        weight (first (shuffle ["200" "300" "400" "500" "600" "700"]))]
    (clerk/html [:div.rounded-full.p-5.w-5
                 {:class (str "bg-" color "-" weight)}])))

^{::clerk/visibility {:result :show}}
(clerk/with-viewer
  {:render-fn `view :require-cljs true :transform-fn clerk/mark-presented}
  13)

(comment
  (count outbox)
  (.put outbox (clerk/present (random-pill)))
  (.poll inbox 3 TimeUnit/SECONDS)
  (.poll outbox 3 TimeUnit/SECONDS)
  (clerk/recompute!)
  )
