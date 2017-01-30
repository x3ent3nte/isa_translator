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

(assert (= post (Execute pre 0 #b0000 0 1 2)))
;(assert (=> (and (= #b1 ((_ extract 0 0) (select pre 16))) (= post)) ()))



(check-sat)

(get-model)
