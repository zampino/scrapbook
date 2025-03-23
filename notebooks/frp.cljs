(ns frp
  (:require [nextjournal.clerk.render :as render]
            [nextjournal.clerk.render.hooks :as hooks]))

(defn view [tick _]
  (let [!state (hooks/use-state {:tick tick :is "paused"})
        tick! (hooks/use-callback #(swap! !state update :tick inc))
        {:keys [pdata error tick is]} @!state]
    (hooks/use-effect
     (fn []
       (.. (render/clerk-eval (list 'poll))
           (then (fn [r]
                   #_(js/console.log (pr-str r) )
                   (js/setTimeout tick! 500)
                   (swap! !state
                          #(-> %
                               (dissoc :error)
                               (assoc :pdata r)))))
           (catch #(swap! !state assoc :error %)))
       :ok) [tick])
    [:div
     [:button.rounded-md.bg-indigo-200.text-xl.px-5.py-1
      {:on-click #(swap! !state update :playing "playing")}
      (if (= is "playing") "⏸" "▶")]
     (cond
       error [:pre.border-red-300 (pr-str error)]
       pdata [render/inspect-presented pdata])]))
