(ns scrapbook.deputy.equality
  (:require
   [deputy.core :as d]
   [deputy.syntax :as s :refer :all]
   [deputy.extensions.lets]))

;; Leibniz indiscernibility of equals

(d/defterm [l= [T :type] [x T] [y T] :type]
  (Π [P (=> T :type)] (=> (P x) (P y))))

(d/defterm [id [T :type] (=> T T)] (fun [x] x))
(d/defterm [∘ [A :type] [B :type] [C :type]
            [f (=> A B)] [g (=> B C)] (=> A C)]
  (fun [a] (g (f a))))

(d/defterm [l=-refl [T :type] [x T] (l= T x x)]
  (fun [P] (id (P x))))

(d/defterm [l=-sym [T :type] [x T] [y T] (=> (l= T x y) (l= T y x))]
  (fun [x=y P]
       (lets [[Q (=> T :type)] (fun [z] (=> (P z) (P x)))]
         (x=y Q ((l=-refl T x) P)))))

(d/defterm [l=-trans [T :type] [x T] [y T] [z T]
            [x=y (l= T x y)] [y=z (l= T y z)] (l= T x z)]
  (fun [P Px] (y=z P (x=y P Px))))

(comment
  (ex-data *e))
