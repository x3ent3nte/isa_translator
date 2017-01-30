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

(define-sort State () (Array Int (_ BitVec 32)))
(define-sort Reg () Int)
(define-sort Cond () (_ BitVec 4))
(define-sort Opcode () String)

(declare-const ZERO (_ BitVec 32))
(assert (= ZERO #b00000000000000000000000000000000))

(declare-const pre State)
(declare-const post State)

(declare-fun Execute3 (State Opcode Cond Reg Reg Reg) State)
(declare-fun Execute2 (State Opcode Cond Reg Reg) State)
(declare-fun EqualXPC (State State) Bool)
(declare-fun EqualXCPSRPC (State State) Bool)
(declare-fun EqualXR0PC (State State) Bool)

(declare-fun EvalCond (State Cond) Bool)

(declare-fun N1 (State) Bool)
(declare-fun Z1 (State) Bool)
(declare-fun C1 (State) Bool)
(declare-fun V1 (State) Bool)

(assert (ite (N1 pre) (= ((_ extract 0 0) (select pre 16)) #b1) (= ((_ extract 0 0) (select pre 16)) #b0)))
(assert (ite (N1 post) (= ((_ extract 0 0) (select post 16)) #b1) (= ((_ extract 0 0) (select post 16)) #b0)))
(assert (ite (Z1 pre) (= ((_ extract 1 1) (select pre 16)) #b1) (= ((_ extract 1 1) (select pre 16)) #b0)))
(assert (ite (Z1 post) (= ((_ extract 1 1) (select post 16)) #b1) (= ((_ extract 1 1) (select post 16)) #b0)))
(assert (ite (C1 pre) (= ((_ extract 2 2) (select pre 16)) #b1) (= ((_ extract 2 2) (select pre 16)) #b0)))
(assert (ite (C1 post) (= ((_ extract 2 2) (select post 16)) #b1) (= ((_ extract 2 2) (select post 16)) #b0)))
(assert (ite (V1 pre) (= ((_ extract 3 3) (select pre 16)) #b1) (= ((_ extract 3 3) (select pre 16)) #b0)))
(assert (ite (V1 post) (= ((_ extract 3 3) (select post 16)) #b1) (= ((_ extract 3 3) (select post 16)) #b0)))

;Conditional constraints
;EQ
(assert (ite (EvalCond pre #b0000) (Z1 pre) (not (Z1 pre))))
;NE
(assert (ite (EvalCond pre #b0001) (not (Z1 pre)) (Z1 pre)))
;CS/HS
(assert (ite (EvalCond pre #b0010) (C1 pre) (not (C1 pre)) ))
;CC/LO
(assert (ite (EvalCond pre #b0011) (not (C1 pre))  (C1 pre)))
;MI
(assert (ite (EvalCond pre #b0100) (N1 pre) (not (N1 pre)) ))
;PL
(assert (ite (EvalCond pre #b0101) (not (N1 pre)) (N1 pre) ))
;VS
(assert (ite (EvalCond pre #b0110) (V1 pre) (not (V1 pre)) ))
;VC
(assert (ite (EvalCond pre #b0111) (not (V1 pre)) (V1 pre) ))
;HI
(assert (ite (EvalCond pre #b1000) (and (C1 pre) (not (Z1 pre))) (or (not (C1 pre)) (Z1 pre))))
;LS
(assert (ite (EvalCond pre #b1001) (or (not (C1 pre)) (Z1 pre)) (and (C1 pre) (not (Z1 pre))) ))
;GE
(assert (ite (EvalCond pre #b1010) (= (N1 pre) (V1 pre)) (not (= (N1 pre) (V1 pre))) ))
;LT
(assert (ite (EvalCond pre #b1011) (not (= (N1 pre) (V1 pre))) (= (N1 pre) (V1 pre)) ))
;GT
(assert (ite (EvalCond pre #b1100) (and (not (Z1 pre)) (= (N1 pre) (V1 pre))) (or (Z1 pre) (not (= (N1 pre) (V1 pre)))) ))
;LE
(assert (ite (EvalCond pre #b1101) (or (Z1 pre) (not (= (N1 pre) (V1 pre)))) (and (not (Z1 pre)) (= (N1 pre) (V1 pre)))))
;AL
(assert (ite (EvalCond pre #b1110) true false))
(assert (ite (EvalCond pre #b1111) true false))



;EqualXPC returns true if two states have identical registers 0-14 and CPSR, so their program counters may be different, else false
(assert (=> (EqualXPC pre post) (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16)) )))
(assert (=> (not (EqualXPC pre post)) (not (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16))) )))

;EqualXCPSRPC returns true if two states have identical registers 0-14, so may have differeny pc / cpsr, else false
(assert (=> (EqualXCPSRPC pre post) (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) )))
(assert (=> (not (EqualXCPSRPC pre post)) (not (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14))) )))

;EqualXR0PC returns true if two states have identical registers 1-14 and CPSR, so R0 and their program counters may be different, else false
(assert (=> (EqualXR0PC pre post) (and (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16)) )))
(assert (=> (not (EqualXR0PC pre post)) (not (and (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16))) )))

;Opcodes
; 0 CMP
; 1 ADD
; 2 SUB
; 3 MOV

;CMP R0 R1
;(assert (forall ((x Reg) (y Reg))(=>  (= (Execute2 pre 0 #b1110 x y) post) (and (=> (= (select pre x) (select pre y)) (Z1 post)) (=> (not (= (select pre x) (select pre y))) (not (Z1 post))) (EqualXCPSRPC pre post)) )))
(assert  (=>  (= (Execute2 pre "CMP" #b1110 0 1) post) (and (=> (= (select pre 0) (select pre 1)) (Z1 post)) (=> (not (= (select pre 0) (select pre 1))) (not (Z1 post))) (EqualXCPSRPC pre post)) ))

;ADDEQ R0 R1 R2
;(assert (forall ((c Cond))(=> (and (= (Execute3 pre 0 c 0 1 2) post) (EvalCond pre c)) (and (= (select post 0) (bvadd (select pre 1) (select pre 2))) (EqualXR0PC pre post) ))) )
(assert (=> (and (= (Execute3 pre "ADD" #b0000 0 1 2) post) (EvalCond pre #b0000)) (and (= (select post 0) (bvadd (select pre 1) (select pre 2))) (EqualXR0PC pre post) ))) 
;(assert (forall ((c Cond)) (=> (and (= (Execute3 pre 0 #b0000 0 1 2) post) (not (EvalCond pre #b0000))) (EqualXPC pre post))) )
(assert (=> (and (= (Execute3 pre "ADD" #b0000 0 1 2) post) (not (EvalCond pre #b0000))) (EqualXPC pre post)))

;MOV R0 R1
;(assert (=> (= Execute pre 3)))

(assert (= (Execute2 pre "CMP" #b1110 0 1) post))
(assert (= (select pre 0) #b00000000000000000000000000000111))
(assert (= (select pre 1) #b00000000000000000000000000000111))

(assert (Z1 post))

;(assert (= (Execute3 pre 1 #b0000 0 1 2) post))
;(assert (= (select pre 1) #b00000000000000000000000000000001))
;(assert (= (select pre 2) #b00000000000000000000000000000001))
;(assert (EvalCond pre #b0000))
;(assert (= (select post 0) #b00000000000000000000000000000011))



;(check-sat-using (then qe smt))
;(check-sat-using (then simplify solve-eqs smt))
(check-sat)
(get-model)

































