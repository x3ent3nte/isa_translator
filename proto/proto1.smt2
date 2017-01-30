
(echo "ARM arch")

;(set-option :fixedpoint.engine datalog)
;(set-logic HORN)

;Sorts
(define-sort Reg () (_ BitVec 32))
(define-sort Flag () (_ BitVec 1))
(define-sort Cond () (_ BitVec 4))

;Registers
(declare-var R0 Reg)
(declare-var R1 Reg)
(declare-var R2 Reg)
(declare-var R3 Reg)
(declare-var R4 Reg)
(declare-var R5 Reg)
(declare-var R6 Reg)
(declare-var R7 Reg)
(declare-var R8 Reg)
(declare-var R9 Reg)
(declare-var R10 Reg)
(declare-var R11 Reg)
(declare-var R12 Reg)

(declare-var SP Reg)
(declare-var LR Reg)
(declare-var PC Reg)

(declare-const ZERO Reg)
(assert (= ZERO #b00000000000000000000000000000000))

;Flags
(declare-var N Flag)
(declare-var Z Flag)
(declare-var C Flag)
(declare-var V Flag)

(declare-var I Flag)
(declare-var F Flag)
(declare-var T Flag)
(declare-var M4 Flag)
(declare-var M3 Flag)
(declare-var M2 Flag)
(declare-var M1 Flag)
(declare-var M0 Flag)

;Conditions
(declare-const EQ Cond)
(declare-const NE Cond)
(declare-const CSHS Cond)
(declare-const CCLO Cond)
(declare-const MI Cond)
(declare-const PL Cond)
(declare-const VS Cond)
(declare-const VC Cond)
(declare-const HI Cond)
(declare-const LS Cond)
(declare-const GE Cond)
(declare-const LT Cond)
(declare-const GT Cond)
(declare-const LE Cond)
(declare-const AL Cond)

;Condition values
(assert (= EQ #b0000))
(assert (= NE #b0001))
(assert (= CSHS #b0010))
(assert (= CCLO #b0011))
(assert (= MI #b0100))
(assert (= PL #b0101))
(assert (= VS #b0110))
(assert (= VC #b0111))
(assert (= HI #b1000))
(assert (= LS #b1001))
(assert (= GE #b1010))
(assert (= LT #b1011))
(assert (= GT #b1100))
(assert (= LE #b1101))
(assert (= AL #b1110))

(declare-fun Eval_Cond (Cond) (Bool))

(assert (=> (= (Eval_Cond EQ) true) (= Z #b1)))
(assert (=> (= (Eval_Cond EQ) false) (= Z #b0)))

(assert (=> (= (Eval_Cond NE) true) (= Z #b0)))
(assert (=> (= (Eval_Cond NE) false) (= Z #b1)))

(assert (=> (= (Eval_Cond CSHS) true) (= C #b1)))
(assert (=> (= (Eval_Cond CSHS) false) (= C #b0)))

(assert (=> (= (Eval_Cond CCLO) true) (= C #b0)))
(assert (=> (= (Eval_Cond CCLO) false) (= C #b1)))

(assert (=> (= (Eval_Cond MI) true) (= N #b1)))
(assert (=> (= (Eval_Cond MI) false) (= N #b0)))

(assert (=> (= (Eval_Cond PL) true) (= N #b0)))
(assert (=> (= (Eval_Cond PL) false) (= N #b1)))

(assert (=> (= (Eval_Cond VS) true) (= V #b1)))
(assert (=> (= (Eval_Cond VS) false) (= V #b0)))

(assert (=> (= (Eval_Cond VC) true) (= V #b0)))
(assert (=> (= (Eval_Cond VC) false) (= V #b1)))

(assert (=> (= (Eval_Cond HI) true) (and (= C #b1) (= Z #b0))))
(assert (=> (= (Eval_Cond HI) false) (not (and (= C #b1) (= Z #b0)))))

(assert (=> (= (Eval_Cond LS) true) (and (= C #b0) (= Z #b1))))
(assert (=> (= (Eval_Cond LS) false) (not (and (= C #b0) (= Z #b1)))))

(assert (=> (= (Eval_Cond GE) true) (= N V)))
(assert (=> (= (Eval_Cond GE) false) (not (= N V))))

(assert (=> (= (Eval_Cond LT) true) (not (= N V))))
(assert (=> (= (Eval_Cond LT) false) (not (not (= N V)))))

(assert (=> (= (Eval_Cond GT) true) (and (= Z #b0) (= N V))))
(assert (=> (= (Eval_Cond GT) false) (not (and (= Z #b0) (= N V)))))

(assert (=> (= (Eval_Cond LE) true) (or (= Z #b1) (not (= N V)))))
(assert (=> (= (Eval_Cond LE) false) (not (or (= Z #b1) (not (= N V))))))

(assert (= (Eval_Cond AL) true))



;(declare-fun State)

;(declare-fun EXECUTE (State Op Cond Reg Reg Reg) (State))

(declare-fun MOV (Reg Reg) Bool)
(assert (forall ((x Reg) (y Reg)) (=> (MOV x y) (= x y))))

(declare-fun ADD (Reg Reg Reg) Bool)
(assert (forall ((x Reg) (y Reg) (z Reg)) (=> (ADD x y z) (= x (bvadd y z)))))

(declare-fun SUB (Reg Reg Reg) Bool)
(assert (forall ((x Reg) (y Reg) (z Reg)) (=> (SUB x y z) (= x (bvsub y z)))))

(declare-fun CMP (Reg Reg) Bool)
(assert  (forall ((x Reg) (y Reg)) (=> (and (CMP x y) (= x y) ) (= Z #b1))))
(assert  (forall ((x Reg) (y Reg)) (=> (and (CMP x y) (not (= x y)) ) (= Z #b0))))

(assert (=> (MOV R0 R1) (= R0 R1)))
(assert (MOV R0 R1))
(assert  (= R0 R1))

(assert (= R0 #b00000000000000000000000000000100))
(assert (= R2 #b00000000000000000000000000000110))

(assert (CMP R0 R2))
(assert (= Z #b0))

(assert (= (Eval_Cond EQ) true))

;(assert (= (bvsub #b001 #b011) 0))





;assert (=> (MOV R0 R1) (= R0 R1)))
;(assert (MOV R0 R1))
;(assert (not (= R0 R1)))



;(declare-rel MOV (Reg Reg))

;(rule (=> (MOV R0 R1) (= R0 R1)))

;(rule (MOV R0 R1))

;(declare-rel q1())
;(rule (=> (MOV R0 R1) q1))


;(assert (not (= R0 R1)))

;(query q1)


(check-sat)

(get-model)

(exit)























