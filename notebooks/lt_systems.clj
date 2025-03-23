;; # 📼 Turing-Lindenmayer Systems
;; also consider turing turtle allitteration
(ns lt-systems
  {:nextjournal.clerk/eval :sci}
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.render.hooks :as hooks]))
;; _This clojurescript notebook ciments into some experiment in turtle graphics
;; combining [Lindenmayer Systems](https://en.wikipedia.org/wiki/L-system) with [Turing machines](https://en.wikipedia.org/wiki/Turing_machine)._
^{::clerk/visibility {:code :hide}}
(clerk/html [:div.flex.justify-end.text-xs {:class "mt-[-3px]"} [:em [:b "Andrea Amantini"]]])


{::clerk/visibility {:result :hide}}
;; We'll first quickly recap turtle graphics and some SVG path basics.
;; We define our Lindenmayer sistem
(def L-System
  {:turtle/stack []
   :turtle/step 10
   :turtle/rho 0
   :turtle/alpha (/ js/Math.PI 6)
   :turtle/cmds []})

(defn start-at [sys [x y :as P]]
  (-> sys
      (assoc :turtle/cursor P)
      (update :turtle/cmds conj (vector "M" x y))))

(defn U [r a] [(* r (js/Math.cos a)) (* r (js/Math.sin a))])

(defn +v [[x y] [x' y']] [(+ x x') (+ y y')])

(defn move-turtle-cursor [{:as state :turtle/keys [cmds]}]
  (let [[_ x y] (peek cmds)]
    (update state :turtle/cursor +v [x y])))

;; Turtle moves on the canvas by means of instructtions encoded in symbols

(defn apply-sym [{:as state :turtle/keys [cursor alpha rho step stack]} sym]
  (condp = sym
    'F (-> state
           (update :turtle/cmds conj (cons "l" (U step rho)))
           move-turtle-cursor)

    #_#_
    'F (-> state
           (update :turtle/cmds conj (cons "m" (U step rho)))
           move-turtle-cursor)

    '+ (update state :turtle/rho + alpha)
    '- (update state :turtle/rho - alpha)
    '< (update state :turtle/stack conj {:cursor cursor :rho rho})
    '> (let [{:keys [cursor rho]} (peek stack)]
         (-> state
             (update :turtle/stack (comp vec butlast))
             (update :turtle/cmds conj (cons "M" cursor))
             (assoc :turtle/cursor cursor
                    :turtle/rho rho)))
    state))

(def read-tape (partial reduce apply-sym))

(defn system->path [{:turtle/keys [cmds] :svg/keys [stroke-width]}]
  [:path
   {:stroke-width (or stroke-width 2)
    :fill "none" :stroke-linecap "round"
    :d (str/join " " (apply concat cmds))}])

(defn draw-L-system [{:as sys :svg/keys [options]}]
  (clerk/html
   [:svg.stroke-sky-500 options
    [system->path sys]]))

(draw-L-system (-> L-System
                   (start-at [100 50])
                   (read-tape '[F F F F - - F F F + + F F F F])
                   (assoc :svg/options {:height 60}
                        :svg/stroke-width 5)))

;; the trick with the stack will help in recursive expansion of the tape
;; `<` records the position at the tape state, while `>` pops the position
;; to the last recorded

(draw-L-system (-> L-System
                   (start-at [100 50])
                   (read-tape '[F F < - - F F + + F F + + F F > + + F F - - F F])
                   (assoc :svg/options {:height 100}
                        :svg/stroke-width 5)))


;; This is how Lyndenmayer systems enter the play, a map of rules prescribes
;; transition from a symbol to a seq of symbols
(defn L-step [{:as sys :keys [rules]}]
  (assert rules)
  (update sys
          :tape (comp vec
                      (partial mapcat (some-fn rules vector))
                      (partial remove #{'_}))))

(defn evolve [sys n]
  (let [sys' (->> (iterate L-step sys) (take n) last)]
    (read-tape sys' (:tape sys'))))

(draw-L-system (-> L-System
                   (start-at [100 150])
                   (assoc :tape '[C])
                   (assoc :rules {'F '[F F]
                                'C '[F < - C > + C]})
                   (evolve 6)
                   (assoc :svg/options {:width 600 :height 300})))

(draw-L-system (-> L-System
                   (start-at [200 400])
                   (assoc :tape '[- - C]
                        :turtle/step 5
                        :rules {'F '[F]
                                'C '[N F < - P - F > F + C]
                                'N '[N F F]
                                'P '[Q]
                                'Q '[C]})
                   (evolve 10)
                   (assoc :svg/options {:width 600
                                      :height 600})))

;; FIXME: penrose tiling isn't working
(draw-L-system (-> L-System
                   (start-at [200 100])
                   (assoc :tape '[ < X > + + < X > + + < X > + + < X > + + < X >]
                        ;;:tape '[+ Y F - - Z F < - - - W F - - X F > +]
                        :turtle/step 20
                        :turtle/alpha (/ (* 2 js/Math.PI) 10)
                        :rules {'W '[Y F + + Z F - - - - X F < - Y F - - - - W F > + +]
                                'X '[+ Y F - - Z F < - - - W F - - X F > +]
                                'Y '[- W F + + X F < + + + Y F + + Z F > -]
                                'Z '[- - Y F + + + + W F < + Z F + + + + X F > - - X F]})
                   (evolve 3)
                   (assoc :svg/options {:width 500
                                        :height 200})))
#_
(->> "--YF++++WF[+ZF++++XF]--XF"
     (interleave (repeat " "))
     (map #(get {\] \> \[ \<} % %))
     (apply str))

(defn step-turtle [{:as sys :keys [tape head]}]
  (apply-sym sys (tape head)))

(defn move-head [s dir]
  (update s :head #(mod (+ % dir) (count (:tape s)))))

(defn step-sys [{:as sys :keys [tape head rules queue dir]}]
  (let [sym (tape head)
        [qhd & queue'] queue]
    (js/console.log :read sym :write qhd)
    (-> sys
        (cond-> qhd
          (update :tape assoc head qhd))
        (assoc :queue
               (concat queue' (when-not (= '_ sym)
                                (or (rules sym) [sym]))))
        (cond-> (= '| sym) (update :dir * -1))
        (move-head dir))))

(def step (comp step-turtle step-sys))

(defn sym-pos [idx c len r]
  (let [alpha (/ (* 2 js/Math.PI idx) len) h 10]
    {:x (+ c (* r (js/Math.cos alpha)))
     :y (+ c (* r (js/Math.sin alpha)) h)}))

(defn head-frame [idx len R d c]
  (let [innerR (- R d)
        outerR (+ R d)
        alpha (/ (* (inc (* 2 idx)) js/Math.PI) len)
        beta (/ (* (dec (* 2 idx)) js/Math.PI) len)
        ax (+ (* innerR (js/Math.cos alpha)) c)
        ay (+ (* innerR (js/Math.sin alpha)) c)
        bx (+ (* outerR (js/Math.cos alpha)) c)
        by (+ (* outerR (js/Math.sin alpha)) c)
        cx (+ (* outerR (js/Math.cos beta)) c)
        cy (+ (* outerR (js/Math.sin beta)) c)
        dx (+ (* innerR (js/Math.cos beta)) c)
        dy (+ (* innerR (js/Math.sin beta)) c)]
    [:path
     {:fill "none" :stroke-width 2 :stroke "currentColor"
      :d (str/join " "
                   [(str "M " ax " " ay)
                    (str "L " bx " " by)
                    (str "A " outerR " " outerR " " 0 " " 0 " " 0 " " cx " " cy)
                    (str "L " dx " " dy)
                    (str "A " innerR " " innerR " " 0 " " 0 " " 1 " " ax " " ay)
                    "Z"])}]))

(defn render-tape [{:as state :keys [head tape]}]
  (let [len (count tape)
        R 80 d 16 c (+ R d 10)]
    (into [:g
           [:circle {:cx c :cy c :r R :stroke-width (* 2 d) :fill "none"}]
           [head-frame head len R d c]]
          (map-indexed (fn [idx sym]
                         (let [{:keys [x y]} (sym-pos idx c len R)]
                           [:text
                            {:style {:font-size d :padding "5 5 auto"}
                             :text-anchor "middle"
                             :fill "currentColor" :stroke "none" :x x :y y} (str sym)])) tape))))

(defn animate [!s]
  (let [I (js/setInterval
           (fn [] (js/requestAnimationFrame #(swap! !s step))) 100)]
    #(js/clearInterval I)))

(defn render-system []
  (let [!state (hooks/use-state
                (-> L-System
                    (start-at [400 100])
                    (assoc :head 0 :dir 1
                           :turtle/step 30
                           :svg/options {:width 600 :height 500}
                           :tape '[C _ _ _ _ _ _ _ _ _ _ _ ]
                           :rules {'F '[ F H ]
                                   'H '[ F F H | +]
                                   'C '[ F < - C > + C]}

                           ;;:tape '[- X - - X _ _ _ _ _ _ _ _ _ _ ]
                           ;;:rules {'F '[F]
                           ;;        'G '[F | + + ]
                           ;;        'X '[X F X < - - - - X G X > + + + + X]}


                           )))]
    (hooks/use-effect (partial animate !state))
    ;; TODO: animate vs controls
    [:svg.font-sans.border.w-fit.stroke-indigo-300.w-96
     {:viewBox "0 0 800 400"}
     [render-tape @!state]
     [system->path @!state]]))

^{::clerk/width :full}
(clerk/html [render-system])
