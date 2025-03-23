;; # 🐢 Primitives
(ns turtle
  {:nextjournal.clerk/eval :sci}
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [reagent.core :as r]
            ))

(def PI  js/Math.PI)
(def cos js/Math.cos)
(def sin js/Math.sin)

(def ^:dynamic *turtle*
  (atom {:turtle/stack []
         :turtle/cursor [0 0]
         :turtle/step 10
         :turtle/rho 0
         :max-steps 100
         :turtle/alpha (/ PI 6)
         :turtle/cmds []}))

(defn deg->rad [deg] (/ (* PI deg) 180))

(defn U [r a] [(* r (cos a)) (* r (sin a))])

(defn +v [[x y] [x' y']] [(+ x x') (+ y y')])

(defn move-turtle-cursor [{:as state :turtle/keys [cmds]}]
  (let [[_ x y] (peek cmds)]
    (update state :turtle/cursor +v [x y])))

(defn left
  ([deg] (swap! *turtle* left deg))
  ([turtle deg] (update turtle :turtle/rho + (deg->rad deg))))

(defn right
  ([deg] (swap! *turtle* right deg))
  ([turtle deg] (update turtle :turtle/rho - (deg->rad deg))))

(defn forward
  ([input] (swap! *turtle* forward input))
  ([{:as turtle :turtle/keys [cursor alpha rho step stack]} input]
   (-> turtle
       (update :turtle/cmds conj (cons "l" (U step rho)))
       move-turtle-cursor)))

(defn turtle->path [{:turtle/keys [cmds] :svg/keys [stroke-width]}]
  [:path
   {:stroke-width (or stroke-width 2)
    :fill "none" :stroke-linecap "round"
    :d (str/join " " (apply concat cmds))}])

(defn draw-turtle [{:as sys :svg/keys [options]}]
  (clerk/html
   [:div.border-sky-500
    [:svg.stroke-sky-500 options
     [turtle->path sys]]]))

(defn poly [side angle]
  (dotimes [_ 10]
    (forward side)
    (right angle)))

(deref *turtle*)

(comment
  (poly 10 10)

  (turtle->path @*turtle*)
  (draw-turtle @*turtle*))
