(echo "Generating ARM V8 Constraints")

;(set-info :status unknown)
;(set-logic HORN)
;(set-option :fixedpoint.engine datalog)
;(set-option :fixedpoint.engine duality)
;(set-option :fixedpoint.engine pdr)

;(set-option :smt.auto-config false)
(set-option :smt.mbqi true)
;(set-option :smt.macro_finder true)
;(set-option :smt.pull-nested-quantifiers true)
;(set-option :smt.mbqi.trace true)

(define-sort State () (Array (_ BitVec 4)(_ BitVec 32)))
(define-sort Reg () (_ BitVec 4))
(define-sort Cond () (_ BitVec 4))
(define-sort Opcode () (_ BitVec 4))
(define-sort Flags () (_ BitVec 4))

(declare-const ZERO (_ BitVec 32))
(assert (= ZERO #b00000000000000000000000000000000))

(declare-const pre State)
(declare-const pre_flags Flags)

(declare-const post State)
(declare-const post_flags Flags)

(declare-fun Execute3 (State Opcode Cond Reg Reg Reg) State)
(declare-fun Execute2 (State Opcode Cond Reg Reg) State)
(declare-fun EqualXPC (State State) Bool)
(declare-fun EqualXCPSRPC (State State) Bool)
(declare-fun EqualXR0PC (State State) Bool)

(declare-fun EvalCond (Flags Cond) Bool)

(declare-fun N1 (Flags) Bool)
(declare-fun Z1 (Flags) Bool)
(declare-fun C1 (Flags) Bool)
(declare-fun V1 (Flags) Bool)

(assert (forall ((fl Flags)) (ite (N1 fl) (= ((_ extract 0 0) fl) #b1) (= ((_ extract 0 0) fl) #b0))))
(assert (forall ((fl Flags)) (ite (Z1 fl) (= ((_ extract 1 1) fl) #b1) (= ((_ extract 1 1) fl) #b0))))
(assert (forall ((fl Flags)) (ite (C1 fl) (= ((_ extract 2 2) fl) #b1) (= ((_ extract 2 2) fl) #b0))))
(assert (forall ((fl Flags)) (ite (V1 fl) (= ((_ extract 3 3) fl) #b1) (= ((_ extract 3 3) fl) #b0))))

;Conditional constraints
;EQ
(assert (forall ((fl Flags)) (ite (EvalCond fl #b0000) (Z1 fl) (not (Z1 fl)))))
;NE
(assert (forall ((fl Flags)) (ite (EvalCond fl #b0001) (not (Z1 fl)) (Z1 fl))))
;CS/HS
(assert (forall ((fl Flags)) (ite (EvalCond fl #b0010) (C1 fl) (not (C1 fl)))))
;CC/LO
(assert (forall ((fl Flags)) (ite (EvalCond fl #b0011) (not (C1 fl)) (C1 fl))))
;MI
(assert (forall ((fl Flags)) (ite (EvalCond fl #b0100) (N1 fl) (not (N1 fl)))))
;PL
(assert (forall ((fl Flags)) (ite (EvalCond fl #b0101) (not (N1 fl)) (N1 fl))))
;VS
(assert (forall ((fl Flags)) (ite (EvalCond fl #b0110) (V1 fl) (not (V1 fl)))))
;VC
(assert (forall ((fl Flags)) (ite (EvalCond fl #b0111) (not (V1 fl)) (V1 fl))))
;HI
(assert (forall ((fl Flags)) (ite (EvalCond fl #b1000) (and (C1 fl) (not (Z1 fl))) (or (not (C1 fl)) (Z1 fl)))))
;LS
(assert (forall ((fl Flags))(ite (EvalCond fl #b1001) (or (not (C1 fl)) (Z1 fl)) (and (C1 fl) (not (Z1 fl))) )))
;GE
(assert (forall ((fl Flags))(ite (EvalCond fl #b1010) (= (N1 fl) (V1 fl)) (not (= (N1 fl) (V1 fl))) )))
;LT
(assert (forall ((fl Flags))(ite (EvalCond fl #b1011) (not (= (N1 fl) (V1 fl))) (= (N1 fl) (V1 fl)) )))
;GT
(assert (forall ((fl Flags))(ite (EvalCond fl #b1100) (and (not (Z1 fl)) (= (N1 fl) (V1 fl))) (or (Z1 fl) (not (= (N1 fl) (V1 fl)))) )))
;LE
(assert (forall ((fl Flags))(ite (EvalCond fl #b1101) (or (Z1 fl) (not (= (N1 fl) (V1 fl)))) (and (not (Z1 fl)) (= (N1 fl) (V1 fl))))))
;AL
(assert (forall ((fl Flags))(ite (EvalCond fl #b1110) true false)))
(assert (forall ((fl Flags))(ite (EvalCond fl #b1111) true false)))



;EqualXPC returns true if two states have identical registers 0-14 and CPSR, so their program counters may be different, else false
;(assert (=> (EqualXPC pre post) (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16)) )))
;(assert (=> (not (EqualXPC pre post)) (not (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16))) )))

;EqualXCPSRPC returns true if two states have identical registers 0-14, so may have differeny pc / cpsr, else false
;(assert (=> (EqualXCPSRPC pre post) (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) )))
;(assert (=> (not (EqualXCPSRPC pre post)) (not (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14))) )))

;EqualXR0PC returns true if two states have identical registers 1-14 and CPSR, so R0 and their program counters may be different, else false
;(assert (=> (EqualXR0PC pre post) (and (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16)) )))
;(assert (=> (not (EqualXR0PC pre post)) (not (and (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16))) )))

;Opcodes
; 0 CMP
; 1 ADD
; 2 SUB
; 3 MOV

;CMP R0 R1
(assert (forall ((x Reg) (y Reg) (c Cond)) (ite  (and (= (Execute2 pre #b0000 c x y) post) (EvalCond pre_flags c)) (and (=> (= (select pre x) (select pre y)) (Z1 post_flags)) (=> (not (= (select pre x) (select pre y))) (not (Z1 post_flags))) ) (and (= pre_flags post_flags) (= pre post)) )))
;(assert  (=>  (= (Execute2 pre 0 #b1110 0 1) post) (and (=> (= (select pre 0) (select pre 1)) (Z1 post)) (=> (not (= (select pre 0) (select pre 1))) (not (Z1 post))) (EqualXCPSRPC pre post)) ))

;ADDEQ R0 R1 R2
;(assert (forall ((c Cond))(=> (and (= (Execute3 pre 0 c 0 1 2) post) (EvalCond pre c)) (and (= (select post 0) (bvadd (select pre 1) (select pre 2))) (EqualXR0PC pre post) ))) )
;(assert (=> (and (= (Execute3 pre 1 #b0000 0 1 2) post) (EvalCond pre #b0000)) (and (= (select post 0) (bvadd (select pre 1) (select pre 2))) (EqualXR0PC pre post) ))) 
;(assert (forall ((c Cond)) (=> (and (= (Execute3 pre 0 #b0000 0 1 2) post) (not (EvalCond pre #b0000))) (EqualXPC pre post))) )
;(assert (=> (and (= (Execute3 pre 1 #b0000 0 1 2) post) (not (EvalCond pre #b0000))) (EqualXPC pre post)))

;MOV R0 R1
;(assert (forall ((x Reg) (y Reg)) () ))


(assert (= (Execute2 pre #b0000 #b1110 #b0010 #b0011) post))
(assert (= (select pre #b0010) #b00000000000000000000000000000111))
(assert (= (select pre #b0011) #b00000000000000000000000000000110))

(assert (not(Z1 post_flags)))

;(assert (= (Execute3 pre 1 #b0000 0 1 2) post))
;(assert (= (select pre 1) #b00000000000000000000000000000001))
;(assert (= (select pre 2) #b00000000000000000000000000000001))
;(assert (EvalCond pre #b0000))
;(assert (= (select post 0) #b00000000000000000000000000000011))



(check-sat-using (then qe smt))
;(check-sat-using (then simplify solve-eqs smt))
;(check-sat)
;(get-model)

































