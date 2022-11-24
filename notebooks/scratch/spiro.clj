;; # 🌀 Clerk Sync Spirograph
(ns scratch.spiro
  {:nextjournal.clerk/no-cache true
   :nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [nextjournal.clerk :as clerk]
            [clojure.math :as math]
            [nextjournal.clerk.viewer :as v]
            [nextjournal.clerk.render.hooks :as-alias render.hooks]
            [clojure.core.async :as async]
            [applied-science.js-interop :as j]))

(def spiro-view
  '(fn svg-drawing [!model]
     (let [ref (render.hooks/use-ref nil)
           center-coordinates (render.hooks/use-state "matrix(1 0 0 1 0 0)")
           ->svg-p (fn [[x y]] (str x "," y))
           ->svg-path (fn [[p & ps :as points]]
                        (when (seq ps)
                          [:path {:fill "none" :stroke "currentColor" :stroke-linecap "round" :stroke-width 5
                                  :d (str "M " (->svg-p p) " L " (clojure.string/join " " (map ->svg-p ps)))}]))]
       (render.hooks/use-effect (fn []
                                  (let [^js bcr (.getBoundingClientRect @ref)]
                                    (reset! center-coordinates (str "matrix(1 0 0 -1 "
                                                                    (js/Math.round (/ (.-width bcr) 2)) " "
                                                                    (js/Math.round (/ (.-height bcr) 2)) ")")))))
       (let [{:keys [path angles]} @!model
             [alpha beta gamma] angles]
         [:svg.text-amber-600.border.border-amber-500.border-4 {:height "600" :width "100%" :ref ref}

          [:g {:transform @center-coordinates}
           [:g {:transform (str "rotate(" alpha ")") :stroke "black" :stroke-linecap "round" :stroke-width 5}
            [:line {:x1 "0" :y1 "0" :x2 "200" :y2 "0"}]]
           [->svg-path path]]]))))

(def spiro-viewer (assoc v/viewer-eval-viewer :render-fn spiro-view))

;;    Model { angles : Vec 3 Radians
;;            phasors : Vec 3 Phasor
;;            path : List Point}
;;
;;    Phasor { frequency : Float,
;;             amplitude : Float}

(def R 100) (def Omega 0.23)
(defn a+ [[x y] [amp alpha]]
  [(+ x (* R amp (math/cos alpha)))
   (+ y (* R amp (math/sin alpha)))])
(defn pen [phasors angles]
  (reduce a+ [0 0] (map (fn [p a] [(:amplitude p) a]) phasors angles)))

(defn update-path [{:as m :keys [phasors angles]}]
  (update m :path conj (pen phasors angles)))

(defn apply-params [{:as model :keys [phasors]} dt]
  (update-path
   (reduce-kv (fn [m i {:keys [frequency]}]
                (update-in m [:angles i] + (* Omega frequency dt)))
              model (zipmap (range) phasors))))

{:nextjournal.clerk/visibility {:code :hide :result :show}}

;; while the model is shared between server and client
^{::clerk/sync true ::clerk/viewer spiro-viewer ::clerk/width :full}
(defonce !model (atom {:angles [0.0 0.0 0.0]
                       :path []
                       :phasors [{:amplitude 1.0 :frequency 0.2}
                                 {:amplitude 0.75 :frequency 0.5}
                                 {:amplitude 0.50 :frequency 0.8}]}))

{:nextjournal.clerk/visibility {:code :hide :result :hide}}

(defn loop-every [ms f]
  (let [ctrl-ch (async/chan)]
    (async/thread
      (loop [n 10000]
        (let [[_ c] (async/alts!! [ctrl-ch (async/timeout ms)])]
          (if (or (= 0 n) (= c ctrl-ch))
            (println "stopping")
            (do (f) (recur (dec n)))))))
    ;; returns a stop-fn
    #(async/close! ctrl-ch)))

(defn reset-drawing! []
  (swap! !model assoc :path [] :angles [0.0 0.0 0.0])
  (clerk/recompute!))

(comment
  (reset-drawing!)

  (def stop-fn
    (loop-every 30 #(do
                      (swap! !model apply-params 0.25)
                      (clerk/recompute!))))

  (stop-fn)

  (do
    #_ (swap! params update-in [:phasors 0] update :amplitude + 1.2)
    #_ (swap! params update-in [:phasors 1] update :frequency + 0.5)
    (swap! !model update-in [:phasors 0] assoc
           :frequency 0.5
           :amplitude 1.0)
    (swap! !model update-in [:phasors 1] assoc
           :frequency 3.8
           :amplitude 0.75)
    (swap! !model update-in [:phasors 2] assoc
           :frequency -1.4
           :amplitude 0.5)
    #_ (swap! params update-in [:phasors 2] update :frequency - 0.5)
    #_ (clerk/recompute!))

  ;; tick
  (do
    (swap! !model apply-params 0.2)
    (clerk/recompute!))

  (swap! !model apply-params 0.2)

  (def stop-fn (loop-work #(println "working")))
  (stop-fn))
