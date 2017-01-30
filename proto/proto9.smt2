(echo "Generating ARM V8 Constraints")

;(set-info :status unknown)
;(set-logic HORN)
;(set-option :fixedpoint.engine datalog)
;(set-option :fixedpoint.engine duality)
;(set-option :fixedpoint.engine pdr)

;(set-option :smt.auto-config false)
;(set-option :smt.mbqi true)
;(set-option :smt.macro_finder true)
;(set-option :smt.pull-nested-quantifiers true)
;(set-option :smt.mbqi.trace true)

(declare-datatypes () ((Reg R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 SP LR PC CPSR)))
(declare-datatypes () ((Cond EQ NE CS CC MI PL VS VC HI LS GE LT GT LE AL)))
(declare-datatypes () ((Op MOV CMP ADD SUB)))
(define-sort State () (Array Reg (_ BitVec 32)))

(declare-const ZERO (_ BitVec 32))
(assert (= ZERO #b00000000000000000000000000000000))

(declare-const FOUR (_ BitVec 32))
(assert (= FOUR #b00000000000000000000000000000100))

(declare-const pre State)
(declare-const post State)

(declare-fun Execute3 (State Op Cond Reg Reg Reg) State)
(declare-fun Execute2 (State Op Cond Reg Reg) State)

(declare-fun PCplus4 (State State) Bool)

(declare-fun EqualXPC (State State) Bool)
(declare-fun EqualXCPSRPC (State State) Bool)
(declare-fun EqualXR0PC (State State) Bool)

(declare-fun EvalCond (State Cond) Bool)

(declare-fun N1 (State) Bool)
(declare-fun Z1 (State) Bool)
(declare-fun C1 (State) Bool)
(declare-fun V1 (State) Bool)

(assert (ite (N1 pre) (= ((_ extract 0 0) (select pre CPSR)) #b1) (= ((_ extract 0 0) (select pre CPSR)) #b0)))
(assert (ite (N1 post) (= ((_ extract 0 0) (select post CPSR)) #b1) (= ((_ extract 0 0) (select post CPSR)) #b0)))
(assert (ite (Z1 pre) (= ((_ extract 1 1) (select pre CPSR)) #b1) (= ((_ extract 1 1) (select pre CPSR)) #b0)))
(assert (ite (Z1 post) (= ((_ extract 1 1) (select post CPSR)) #b1) (= ((_ extract 1 1) (select post CPSR)) #b0)))
(assert (ite (C1 pre) (= ((_ extract 2 2) (select pre CPSR)) #b1) (= ((_ extract 2 2) (select pre CPSR)) #b0)))
(assert (ite (C1 post) (= ((_ extract 2 2) (select post CPSR)) #b1) (= ((_ extract 2 2) (select post CPSR)) #b0)))
(assert (ite (V1 pre) (= ((_ extract 3 3) (select pre CPSR)) #b1) (= ((_ extract 3 3) (select pre CPSR)) #b0)))
(assert (ite (V1 post) (= ((_ extract 3 3) (select post CPSR)) #b1) (= ((_ extract 3 3) (select post CPSR)) #b0)))

;Conditional constraints
;EQ
(assert (ite (EvalCond pre EQ) (Z1 pre) (not (Z1 pre))))
;NE
(assert (ite (EvalCond pre NE) (not (Z1 pre)) (Z1 pre)))
;CS/HS
(assert (ite (EvalCond pre CS) (C1 pre) (not (C1 pre)) ))
;CC/LO
(assert (ite (EvalCond pre CC) (not (C1 pre))  (C1 pre)))
;MI
(assert (ite (EvalCond pre MI) (N1 pre) (not (N1 pre)) ))
;PL
(assert (ite (EvalCond pre PL) (not (N1 pre)) (N1 pre) ))
;VS
(assert (ite (EvalCond pre VS) (V1 pre) (not (V1 pre)) ))
;VC
(assert (ite (EvalCond pre VC) (not (V1 pre)) (V1 pre) ))
;HI
(assert (ite (EvalCond pre HI) (and (C1 pre) (not (Z1 pre))) (or (not (C1 pre)) (Z1 pre))))
;LS
(assert (ite (EvalCond pre LS) (or (not (C1 pre)) (Z1 pre)) (and (C1 pre) (not (Z1 pre))) ))
;GE
(assert (ite (EvalCond pre GE) (= (N1 pre) (V1 pre)) (not (= (N1 pre) (V1 pre))) ))
;LT
(assert (ite (EvalCond pre LT) (not (= (N1 pre) (V1 pre))) (= (N1 pre) (V1 pre)) ))
;GT
(assert (ite (EvalCond pre GT) (and (not (Z1 pre)) (= (N1 pre) (V1 pre))) (or (Z1 pre) (not (= (N1 pre) (V1 pre)))) ))
;LE
(assert (ite (EvalCond pre LE) (or (Z1 pre) (not (= (N1 pre) (V1 pre)))) (and (not (Z1 pre)) (= (N1 pre) (V1 pre)))))
;AL
(assert (ite (EvalCond pre AL) true false))

(assert (ite (PCplus4 pre post) (= (bvadd (select pre PC) FOUR) (select post PC)) (not (= (bvadd (select pre PC) FOUR) (select post PC)))))

;EqualXPC returns true if two states have identical registers 0-14 and CPSR, so their program counters may be different, else false
(assert (=> (EqualXPC pre post) (and (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR)) (= (select pre CPSR) (select post CPSR)) )))
(assert (=> (not (EqualXPC pre post)) (not (and (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR)) (= (select pre CPSR) (select post CPSR)) ) )))

;EqualXCPSRPC returns true if two states have identical registers 0-14, so may have differeny pc / cpsr, else false
(assert (=> (EqualXCPSRPC pre post) (and (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))  )))
(assert (=> (not (EqualXCPSRPC pre post)) (not (and (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR)) ) )))

;EqualXR0PC returns true if two states have identical registers 1-14 and CPSR, so R0 and their program counters may be different, else false
(assert (=> (EqualXR0PC pre post) (and  (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR)) (= (select pre CPSR) (select post CPSR)) )))
(assert (=> (not (EqualXR0PC pre post)) (not (and (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR)) (= (select pre CPSR) (select post CPSR)) ) )))

;CMP R0 R1
(assert  (=>  (and (= (Execute2 pre CMP AL R0 R1) post) (EvalCond pre AL)) (and (=> (= (select pre R0) (select pre R1)) (Z1 post)) (=> (not (= (select pre R0) (select pre R1))) (not (Z1 post))) (EqualXCPSRPC pre post) (PCplus4 pre post)) ))
;(assert (forall ((x Reg) (y Reg)) (=>  (= (Execute2 pre CMP AL x y) post) (and (=> (= (select pre x) (select pre y)) (Z1 post)) (=> (not (= (select pre x) (select pre y))) (not (Z1 post))) (EqualXCPSRPC pre post)) ) ) )
;ADDEQ R0 R1 R2
(assert (=> (and (= (Execute3 pre ADD EQ R0 R1 R2) post) (EvalCond pre EQ)) (and (= (select post R0) (bvadd (select pre R1) (select pre R2))) (EqualXR0PC pre post) ))) 
(assert (=> (and (= (Execute3 pre ADD EQ R0 R1 R2) post) (not (EvalCond pre EQ))) (EqualXPC pre post)))

;MOV R0 R1
;(assert (=> (= Execute pre 3)))

(assert (= (Execute2 pre CMP AL R0 R1) post))
(assert (= (select pre R0) #b00000000000000000000000000000111))
(assert (= (select pre R1) #b00000000000000000000000000000111))
(assert  (Z1 post))

;(assert (= (select pre PC) (select post PC)))




(check-sat)
;(check-sat-using (then qe smt))
;(check-sat-using (then simplify solve-eqs smt))
;(check-sat)
;(get-model)




























