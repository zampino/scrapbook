(ns scratch.clerk-frp
  (:require
   [missionary.core :as m]
   [nextjournal.clerk :as clerk]
   [nextjournal.clerk.viewer :as v]))

(def clock-control
  (assoc v/viewer-eval-viewer
         :render-fn '(fn [v _]
                       (js/console.log "v" (pr-str v))
                       [:div
                        [:button.rounded.bg-sky-500 {:on-click (fn [e] (swap! v inc))} "Advance"]
                        [:em "Ahoi" @v]])))

^{::clerk/sync true
  ::clerk/viewer clock-control}
(defonce ClockTick (atom 0))

(def init {:value 0})

(defn action [m t]
  (prn :t t)
  (update m :value + t))

(defn pull []
  (Thread/sleep 500)
  123
  )

(def outbox-viewer
  (assoc v/viewer-eval-viewer
         :render-fn
         '(fn [x opts]
            (let [outbox ])
            (nextjournal.clerk.render.hooks/use-effect
             (fn [] (nextjournal.clerk.render/clerk-eval (list 'pull)))
             )
            [nextjournal.clerk.render/inspect-presented opts @x])))

^{::clerk/viewer outbox-viewer}
(defonce OutBox (atom init))

(def model-flow
  (let [<tick (m/signal (m/watch ClockTick))]
    (m/reductions action init <tick)))

(def main-task
  (m/reduce
   (fn [_ x]
     (prn :x x)
     (reset! OutBox (clerk/present x)))
   nil
   model-flow))

(comment
  @ClockTick
  (swap! ClockTick inc)



  (def dispose!
    (main-task
     #(prn ::success %)
     #(prn ::crash %)))

  (dispose!)
  )
