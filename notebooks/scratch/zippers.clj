;; # 🎞 Processing Zippers with Tapes
(ns scratch.zippers
  {:nextjournal.clerk/no-cache true}
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.zip :as z]
            [arrowic.core :as a]
            ))

;; region zipper viewers
{::clerk/visibility {:result :hide :code :hide}}
(do
  (defn loc-seq [zloc]
    ;; this sorts nodes so that children seqs are displayed in the correct order by arrowic
    (try
      (doall
       (concat
        (reverse (take-while some? (next (iterate z/prev zloc))))
        (cons zloc
              (take-while (complement z/end?) (next (iterate z/next zloc))))))
      (catch Throwable e
        (throw (ex-info "Cant seq at loc" {:zloc zloc} e)))))
  (def ->name (comp :name z/node))
  (defn pnode [zloc] (some-> zloc (get 1) :pnodes peek))
  (def empty-graph {:vertices #{} :edges []})
  (defn add-v [g zloc] (update g :vertices conj (->name zloc)))
  (defn add-e [g zloc]
    (let [parent-name (-> zloc pnode :name)]
      (cond-> g parent-name (update :edges conj [parent-name (-> zloc z/node :name)]))))
  (defn ->graph [zloc]
    (reduce #(-> %1 (add-e %2) (add-v %2))
            (assoc empty-graph :current? #{(->name zloc)})
            (loc-seq zloc)))
  (defn insert-vtx [{:keys [current?]} name]
    (doto (a/insert-vertex! name
                            :font-color "black"
                            :fill-color (if (current? name) "#ec4899" "#a855f7")
                            :perimeter-spacing 1 :spacing-bottom 1 :spacing-left 1
                            :font-size 20 :font-style 1)
      (.. getGeometry (setWidth 40))
      (.. getGeometry (setHeight 40))))
  (defn ->svg [{:as g :keys [vertices edges]}]
    (a/as-svg
     (a/with-graph (a/create-graph)
                   (let [vmap (zipmap vertices (map (partial insert-vtx g) vertices))]
                     (doseq [[v1 v2] edges]
                       (a/insert-edge! (vmap v1) (vmap v2)
                                       :end-arrow false :rounded true
                                       :stroke-width "3"
                                       :stroke-color "#7c3aed"))))))

  (def zipper?
    (every-pred vector? (comp #{2} count) (comp map? first)
                (comp (some-fn nil? :changed? :ppath :pnodes)
                      second)))

  (def zip-location-viewer
    {:transform-fn (comp nextjournal.clerk.viewer/html (v/update-val #(-> % ->graph ->svg)))
     :pred zipper?})


  (def zip-reel-viewer
    {:pred (every-pred zipper? (comp :cut? meta))
     :transform-fn (comp v/mark-presented (v/update-val (comp #(mapv (comp ->svg ->graph) %) :frames meta)))
     :render-fn '(fn [frames]
                   (let [!state (nextjournal.clerk.render.hooks/use-state {:reel? false :idx 0 :tmr nil})
                         frame-count (count frames)]
                     (let [{:keys [reel? idx tmr]} @!state]
                       (cond
                         (and reel? (not tmr))
                         (swap! !state assoc :tmr (js/setInterval (fn [_] (swap! !state update :idx (comp #(mod % frame-count) inc))) 500))
                         (and (not reel?) tmr)
                         (do (js/clearInterval tmr) (swap! !state assoc :tmr nil :idx 0)))
                       [:div.flex.flex-col
                        [:div.flex.mr-5 {:style {:font-size "1.5rem"}}
                         [:div.border.rounded-full.font-sf.text-xs.cursor-pointer.px-2.py-1.mb-2 {:on-click #(swap! !state update :reel? not)} ({true "⏹ stop" false "▶︎ play"} reel?)]]
                        [nextjournal.clerk.viewer/html (frames (if reel? idx (dec frame-count)))]])))})
  (defn reset-reel [zloc] (vary-meta zloc assoc :frames [] :cut? false))
  (defn add-frame [zloc] (vary-meta zloc update :frames (fnil conj []) zloc))
  (defn cut [zloc] (vary-meta zloc assoc :cut? true))
  (defmacro zmov-> [subj & ops]
    (list* '-> subj `reset-reel `add-frame
           (concat (interpose `add-frame ops) [`add-frame `cut]))))

(clerk/add-viewers! [#_
                     zip-reel-viewer
                     zip-location-viewer])

{::clerk/visibility {:code :show :result :hide}}

(defn ->node
  ([] (->node (symbol (str "n" (swap! !idx inc)))))
  ([name] {:name name}))

(defn ->zip  [r]
  #_ (reset! !idx 0)
  (z/zipper map? :content #(assoc %1 :content %2) r))

;; endregion
{::clerk/visibility {:result :show}}

(def !idx (atom 0))

^{::clerk/visibility {:result :hide}}
(def down*
  (some-fn z/down
           (comp z/down
                 #(z/append-child % (->node)))))

(def up*
  (some-fn z/up
           #(-> (->zip (->node)) (z/insert-child (z/root %)))))


(defn top? [x] (empty? (z/path x)))

(defn left* [loc]
  (or (z/left loc)
      (-> loc
          (cond-> (top? loc) (-> up* down*))
          (z/insert-left (->node)) left*)))

(defn right* [loc]
  (or (z/right loc)
      (-> loc
          (cond-> (top? loc) (-> up* down*))
          (z/insert-right (->node)) right*)))

(-> (->zip (->node '🏠))
    down* up* up* right* left*)

#_
(-> (->zip (->node '🏠))

    right*
    up* down* right* right* right*
    left* left* left*
    )


(def actions
  {\↑ up*
   \↓ down*
   \← left*
   \→ right*})

(def axioms
  {\↑ "↓→"
   \↓ "↑←←"
   \← "→←←"
   \→ "←↑"})

(defn step [s] (apply str (mapcat axioms s)))

(def init "→↑")

(reduce (fn [z dir] ((actions dir) z))
        (->zip (->node '🏠))
        (last (take 5 (iterate step init))))
