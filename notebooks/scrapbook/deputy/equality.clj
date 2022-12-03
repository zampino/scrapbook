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
            [g (=> B C)] [f (=> A B)] (=> A C)]
  (fun [a] (g (f a))))

(d/defterm [l=-refl [T :type] [x T] (l= T x x)]
  (fun [P] (id (P x))))

(d/defterm [l=-sym [T :type] [x T] [y T] (=> (l= T x y) (l= T y x))]
  (fun [x=y P]
       (lets [[Q (=> T :type)] (fun [z] (=> (P z) (P x)))]
         (x=y Q ((l=-refl T x) P)))))

(d/defterm [l=-trans [T :type] [x T] [y T] [z T]
            [x=y (l= T x y)] [y=z (l= T y z)] (l= T x z)]
  #_ (fun [P Px] (y=z P (x=y P Px)))
  (fun [P] (∘ (P x) (P y) (P z) (y=z P) (x=y P))))

(d/defterm [l=-cong [A :type] [B :type]
            [f (=> A B)] [a1 A] [a2 A]
            [a1=a2 (l= A a1 a2)] (l= B (f a1) (f a2))]
  (fun [P Pfa1] (a1=a2 (∘ A B :type P f) Pfa1)))

#_
(defmacro l=-chain [T t1 & terms])
;; (l=> T t1 < p1 > t2 < p2 > t3) : (l= T t1 t3)
;; (l=-trans T x y z p1 p2)
;; (l=> T t1 < p1 > t2 < p2 > t3 <p3> t4)
;; (l=-trans T t1 t2 t3 p1 p2)
;; (l=> T t1 < (l=> T t1 < p1 > t2 < p2 > t3) > t3 <p3> t4)

(d/defterm [≗ [A :type] [B :type] [f (=> A B)] [g (=> A B)] :type]
  (Π [a A] (l= B (f a) (g a))))



(comment
  (ex-data *e))
