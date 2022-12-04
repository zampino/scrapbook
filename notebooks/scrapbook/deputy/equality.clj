(ns scrapbook.deputy.equality
  (:require
   [deputy.core :as d]
   [deputy.syntax :as s :refer :all]
   [deputy.extensions.lets]))

;; Leibniz indiscernibility of equals

(d/defterm [≡ [T :type] [x T] [y T] :type]
  (Π [P (=> T :type)] (=> (P x) (P y))))

(d/defterm [id [T :type] (=> T T)] (fun [x] x))
(d/defterm [∘ [A :type] [B :type] [C :type]
            [g (=> B C)] [f (=> A B)] (=> A C)]
  (fun [a] (g (f a))))

(d/defterm [≡-refl [T :type] [x T] (≡ T x x)]
  (fun [P] (id (P x))))

(d/defterm [≡-sym [T :type] [x T] [y T] (=> (≡ T x y) (≡ T y x))]
  (fun [x=y P]
       (lets [[Q (=> T :type)] (fun [z] (=> (P z) (P x)))]
         (x=y Q ((≡-refl T x) P)))))

(d/defterm [≡-trans [T :type] [x T] [y T] [z T]
            [x=y (≡ T x y)] [y=z (≡ T y z)] (≡ T x z)]
  #_ (fun [P Px] (y=z P (x=y P Px)))
  (fun [P] (∘ (P x) (P y) (P z) (y=z P) (x=y P))))

(d/defterm [≡-cong [A :type] [B :type]
            [f (=> A B)] [a1 A] [a2 A]
            [a1=a2 (≡ A a1 a2)] (≡ B (f a1) (f a2))]
  (fun [P Pfa1] (a1=a2 (∘ A B :type P f) Pfa1)))

(comment
  (defmacro ≡-chain [T t1 & terms]))
;;     (≡> T t1 < p1 > t2 < p2 > t3) : (≡ T t1 t3)
;;     (≡-trans T x y z p1 p2)
;;     (≡> T t1 < p1 > t2 < p2 > t3 <p3> t4)
;;     (≡-trans T t1 t2 t3 p1 p2)
;;     step
;;     (≡> T t1 < (≡> T t1 < p1 > t2 < p2 > t3) > t3 <p3> t4)
;;
;; Pointwise equality
(d/defterm [≗ [A :type] [B :type] [f (=> A B)] [g (=> A B)] :type]
  (Π [a A] (≡ B (f a) (g a))))



(comment
  (ex-data *e))
