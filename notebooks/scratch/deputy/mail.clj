(ns scratch.deputy.mail)

;; I've finally been playing with deputy which is amazing, I have some questions:
;; 1) which sources can I consult about the depently typed "rec/fix" construction for (non-structural?) recursion
;; and what is the design behind avoiding structural recursion and pattern matching in term definition

;; 2) can we define an emtpy \bot type via defdata, or do we have to construct
;; 3) do we get an inductive Equality as indexed data type
;; à la Agda
;; data _≡_ {a} {A : Set a} (x : A) : A → Set a where
;;  instance refl : x ≡ x

;; 4) defdata only works if we :refer :all of the deputy.syntax
;; complains about stuff like Π not being in scope otherwise

;; 5) Implicit typing à la Agda
;;
;; 6) domain/codomain swapped?
(d/defterm [set [A :type] :type] (=> A :type))
(-> set :vtype)
;; => {:node :pi, :domain :type, :codomain {:node :bind, :name A, :body :type}}
