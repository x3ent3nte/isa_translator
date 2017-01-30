(echo "ARM V8")

;(set-option :fixedpoint.engine datalog)
;(set-logic HORN)

(define-sort State () (Array (_ BitVec 4) (_ BitVec 32)))
(define-sort Reg () (_ BitVec 4))
;(define-sort Reg (_ BitVec 32))
;(define-sort Flag (_ BitVec 1))
(define-sort Cond () (_ BitVec 4))
(define-sort OpCode () (_ BitVec 4))

(declare-const ZERO (_ BitVec 32))
(assert (= ZERO #b00000000000000000000000000000000))

(declare-fun Eval_Cond (State Cond) Bool)

(assert (forall ((s State)) (=> (= (Eval_Cond s #b0000) true) (= ((_ extract 0 0) (select s #b1111)) #b1)) ))
;(assert (forall ((s State)) (=> (= (Eval_Cond s #b0000) false) (= ((_ extract 0 0) (select s #b1111)) #b0)) ))

;(assert (forall ((s State)) (=> (= ((_ extract 0 0) (select s #b1111)) #b1) (= (Eval_Cond s #b0000) true))))
;(assert (forall ((s State)) (=> (= (Eval_Cond s #b0000) true) (= ((_ extract 0 0) (select s #b1111)) #b1))))
;(assert (forall ((s State)) (=> (= (Eval_Cond s #b0000) false) (= ((_ extract 0 0) (select s #b1111)) #b0))))



(declare-fun Execute (State OpCode Cond Reg Reg Reg) State)
(assert (forall ((c Cond) (s State) (o OpCode) (x Reg) (y Reg) (z Reg)) (=> (= (Eval_Cond s c) false) (= s (Execute s o c x y z)))))



(declare-const S0 State)
(declare-const S1 State)
(declare-const Nope Cond)

(assert (= (Eval_Cond S0 Nope) true))
(assert (not (= (Execute S0 #b0001 Nope #b0000 #b0001 #b0010) S1)))


(check-sat)

(get-model)
