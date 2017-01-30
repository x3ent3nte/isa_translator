(echo "Generating ARM V8 Constraints")

(define-sort State () (Array Int (_ BitVec 32)))
(define-sort Reg () Int)
(define-sort Flags () (_ BitVec 4) )
(define-sort Cond () (_ BitVec 4))
(define-sort Opcode () Int)

(declare-const ZERO (_ BitVec 32))
(assert (= ZERO #b00000000000000000000000000000000))

(declare-const pre State)
(declare-const post State)

(declare-fun Execute (State Opcode Cond Reg Reg Reg) State)

(declare-fun EqualXpc (State State) Bool)
(assert (=> (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16)) ) (= (EqualXpc pre post) true)))
(assert (=> (= (EqualXpc pre post) true) (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16)) )))
(assert (=> (not (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16)) )) (= (EqualXpc pre post) false)))
(assert (=> (= (EqualXpc pre post) false) (not (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16)) )) ))

;(assert (forall ((o Opcode) (c Cond) (x Reg) (y Reg) (z Reg)) (= (Execute pre 0 c x y z ) post)))
(assert (=> (= #b1 ((_ extract 1 1) (select pre 16))) (= (select (Execute pre 0 #b0000 0 1 2) 0) (bvadd (select pre 1) (select pre 2)) )))
(assert (=> (= (select (Execute pre 0 #b0000 0 1 2) 0) (bvadd (select pre 1) (select pre 2)) ) (= #b1 ((_ extract 1 1) (select pre 16))) ))
(assert (=> (= #b0 ((_ extract 1 1) (select pre 16))) (= (EqualXpc (Execute pre 0 #b0000 0 1 2) pre) true )))
(assert (=> (= (EqualXpc (Execute pre 0 #b0000 0 1 2) pre) true ) (= #b0 ((_ extract 1 1) (select pre 16))) ))

(assert (= ((_ extract 1 1) (select pre 16)) #b0))
(assert (= #b00000000000000000000000000000001 (select pre 1)))
(assert (= #b00000000000000000000000000000001 (select pre 2)))

(assert (= (Execute pre 0 #b0000 0 1 2) post))

(assert (= (select pre 0) #b00000000000000000000000000000111))
(assert (= (select post 0) #b00000000000000000000000000000011))

(check-sat)

(get-model)
