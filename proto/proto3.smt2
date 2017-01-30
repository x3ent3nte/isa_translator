(echo "Creating constrants for ARM-V8")

(set-option :fixedpoint.engine datalog)
;(set-logic HORN)

(define-sort State () (Array Int (_ BitVec 32)))
(define-sort Reg () Int)
(define-sort Flags () (_ BitVec 4))
(define-sort Cond () (_ BitVec 4))
(define-sort Opcode () (_ BitVec 4))

(declare-const ZERO (_ BitVec 32))
(assert (= ZERO #b00000000000000000000000000000000))

(declare-var current State)
(declare-var pred State)



;(declare-fun EvalCond (Cond Flags) Bool)
;(assert (= (EvalCond #b0000 #b1000) true))
;(assert (= (EvalCond #b0000 #b1001) true))
;(assert (= (EvalCond #b0000 #b1010) true))
;(assert (= (EvalCond #b0000 #b1100) true))
;(assert (= (EvalCond #b0000 #b1110) true))
;(assert (= (EvalCond #b0000 #b1101) true))
;(assert (= (EvalCond #b0000 #b1011) true))
;(assert (= (EvalCond #b0000 #b1111) true))

(declare-rel TrueCond (Cond Flags))

;EQ
(rule (TrueCond #b0000 #b1000))
(rule (TrueCond #b0000 #b1001))
(rule (TrueCond #b0000 #b1010))
(rule (TrueCond #b0000 #b1100))
(rule (TrueCond #b0000 #b1110))
(rule (TrueCond #b0000 #b1101))
(rule (TrueCond #b0000 #b1011))
(rule (TrueCond #b0000 #b1111))

(rule (not (TrueCond #b0000 #b0000)))
(rule (not (TrueCond #b0000 #b0001)))
(rule (not (TrueCond #b0000 #b0010)))
(rule (not (TrueCond #b0000 #b0100)))
(rule (not (TrueCond #b0000 #b0110)))
(rule (not (TrueCond #b0000 #b0101)))
(rule (not (TrueCond #b0000 #b0011)))
(rule (not (TrueCond #b0000 #b0111)))

;NE
(rule (TrueCond #b0001 #b0000))
(rule (TrueCond #b0001 #b0001))
(rule (TrueCond #b0001 #b0010))
(rule (TrueCond #b0001 #b0100))
(rule (TrueCond #b0001 #b0110))
(rule (TrueCond #b0001 #b0101))
(rule (TrueCond #b0001 #b0011))
(rule (TrueCond #b0001 #b0111))

(rule (not (TrueCond #b0001 #b1000)))
(rule (not (TrueCond #b0001 #b1001)))
(rule (not (TrueCond #b0001 #b1010)))
(rule (not (TrueCond #b0001 #b1100)))
(rule (not (TrueCond #b0001 #b1110)))
(rule (not (TrueCond #b0001 #b1101)))
(rule (not (TrueCond #b0001 #b1011)))
(rule (not (TrueCond #b0001 #b1111)))

;Al

(rule (TrueCond #b1110 #b1000))
(rule (TrueCond #b1110 #b1001))
(rule (TrueCond #b1110 #b1010))
(rule (TrueCond #b1110 #b1100))
(rule (TrueCond #b1110 #b1110))
(rule (TrueCond #b1110 #b1101))
(rule (TrueCond #b1110 #b1011))
(rule (TrueCond #b1110 #b1111))

(rule (TrueCond #b1110 #b0000))
(rule (TrueCond #b1110 #b0001))
(rule (TrueCond #b1110 #b0010))
(rule (TrueCond #b1110 #b0100))
(rule (TrueCond #b1110 #b0110))
(rule (TrueCond #b1110 #b0101))
(rule (TrueCond #b1110 #b0011))
(rule (TrueCond #b1110 #b0111))

;N Z C V
(declare-fun EvalCond (Cond Flags) Bool)


;EQ
(assert (= (EvalCond #b0000 #b0100) true))
(assert (= (EvalCond #b0000 #b0101) true))
(assert (= (EvalCond #b0000 #b0110) true))
(assert (= (EvalCond #b0000 #b0100) true))
(assert (= (EvalCond #b0000 #b0110) true))
(assert (= (EvalCond #b0000 #b0101) true))
(assert (= (EvalCond #b0000 #b0111) true))
(assert (= (EvalCond #b0000 #b0111) true))

(assert (= (EvalCond #b0000 #b0000) false))
(assert (= (EvalCond #b0000 #b0001) false))
(assert (= (EvalCond #b0000 #b0010) false))
(assert (= (EvalCond #b0000 #b1000) false))
(assert (= (EvalCond #b0000 #b1010) false))
(assert (= (EvalCond #b0000 #b1001) false))
(assert (= (EvalCond #b0000 #b0011) false))
(assert (= (EvalCond #b0000 #b1011) false))


;NE
(assert (= (EvalCond #b0001 #b0100) false))
(assert (= (EvalCond #b0001 #b0101) false))
(assert (= (EvalCond #b0001 #b0110) false))
(assert (= (EvalCond #b0001 #b0100) false))
(assert (= (EvalCond #b0001 #b0110) false))
(assert (= (EvalCond #b0001 #b0101) false))
(assert (= (EvalCond #b0001 #b0111) false))
(assert (= (EvalCond #b0001 #b0111) false))

(assert (= (EvalCond #b0001 #b0000) true))
(assert (= (EvalCond #b0001 #b0001) true))
(assert (= (EvalCond #b0001 #b0010) true))
(assert (= (EvalCond #b0001 #b1000) true))
(assert (= (EvalCond #b0001 #b1010) true))
(assert (= (EvalCond #b0001 #b1001) true))
(assert (= (EvalCond #b0001 #b0011) true))
(assert (= (EvalCond #b0001 #b1011) true))


;CS
(assert (= (EvalCond #b0010 #b0010) true))
(assert (= (EvalCond #b0010 #b0011) true))
(assert (= (EvalCond #b0010 #b0110) true))
(assert (= (EvalCond #b0010 #b1010) true))
(assert (= (EvalCond #b0010 #b1110) true))
(assert (= (EvalCond #b0010 #b1011) true))
(assert (= (EvalCond #b0010 #b0111) true))
(assert (= (EvalCond #b0010 #b1111) true))

(assert (= (EvalCond #b0010 #b0000) false))
(assert (= (EvalCond #b0010 #b0001) false))
(assert (= (EvalCond #b0010 #b0100) false))
(assert (= (EvalCond #b0010 #b1000) false))
(assert (= (EvalCond #b0010 #b1100) false))
(assert (= (EvalCond #b0010 #b1001) false))
(assert (= (EvalCond #b0010 #b0101) false))
(assert (= (EvalCond #b0010 #b1101) false))

;CC
(assert (= (EvalCond #b0011 #b0010) false))
(assert (= (EvalCond #b0011 #b0011) false))
(assert (= (EvalCond #b0011 #b0110) false))
(assert (= (EvalCond #b0011 #b1010) false))
(assert (= (EvalCond #b0011 #b1110) false))
(assert (= (EvalCond #b0011 #b1011) false))
(assert (= (EvalCond #b0011 #b0111) false))
(assert (= (EvalCond #b0011 #b1111) false))

(assert (= (EvalCond #b0011 #b0000) true))
(assert (= (EvalCond #b0011 #b0001) true))
(assert (= (EvalCond #b0011 #b0100) true))
(assert (= (EvalCond #b0011 #b1000) true))
(assert (= (EvalCond #b0011 #b1100) true))
(assert (= (EvalCond #b0011 #b1001) true))
(assert (= (EvalCond #b0011 #b0101) true))
(assert (= (EvalCond #b0011 #b1101) true))

;AL
(assert (= (EvalCond #b1110 #b0000) true))
(assert (= (EvalCond #b1110 #b0001) true))
(assert (= (EvalCond #b1110 #b0010) true))
(assert (= (EvalCond #b1110 #b0100) true))
(assert (= (EvalCond #b1110 #b0110) true))
(assert (= (EvalCond #b1110 #b0101) true))
(assert (= (EvalCond #b1110 #b0011) true))
(assert (= (EvalCond #b1110 #b0111) true))

(assert (= (EvalCond #b1110 #b1000) true))
(assert (= (EvalCond #b1110 #b1001) true))
(assert (= (EvalCond #b1110 #b1010) true))
(assert (= (EvalCond #b1110 #b1100) true))
(assert (= (EvalCond #b1110 #b1110) true))
(assert (= (EvalCond #b1110 #b1101) true))
(assert (= (EvalCond #b1110 #b1011) true))
(assert (= (EvalCond #b1110 #b1111) true))


(assert (= (EvalCond #b0000 #b0000) false))


(declare-fun Execute (State Opcode Cond Reg Reg Reg) State)
(assert (forall ((s State) (o Opcode) (c Cond) (x Reg) (y Reg) (z Reg)) (=> (= (EvalCond c ((_ extract 3 0) (select s 17))) false) (= (Execute s o c x y z) s))))
(assert (forall ((s State) (o Opcode) (c Cond) (x Reg) (y Reg) (z Reg)) (=> (= (EvalCond c ((_ extract 3 0) (select s 17))) true) (= (Execute s o c x y z) s))))

(declare-const S0 State)
(declare-const S1 State)
(assert (not (= S0 S1)))

(assert (= ((_ extract 3 0) (select S0 17)) #b0100))

(assert (= (Execute S0 #b0110 #b0000 0 1 2) S1))

;(assert (forall ((s State)) (=> (= ((_ extract 0 0) (select s 17)) #b1) (= (EvalCond #b0000 s) true))))
;(assert (forall ((s State)) (=> (= ((_ extract 0 0) (select s 17)) #b0) (= (EvalCond #b0000 s) false))))

;(declare-fun Execute (State Opcode Cond Reg Reg Reg) State)

;(assert (forall ((s State) (o Opcode) (x Reg) (y Reg) (z Reg)) (=> (= ((_ extract 0 0) (select s 17)) #b1) (= (Execute s o #b0000 x y z) s))))

;(declare-const S0 State)
;(declare-const S1 State)
;(assert (not (= S0 S1)))
;(assert (= (Execute S0 #b0101 #b0000 0 1 2) S1))

(check-sat)

(get-model)
