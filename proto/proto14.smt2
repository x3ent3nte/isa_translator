(echo "Generating ARM contraints")

(declare-datatypes () ((Operation ADD SUB MOV MVN CMP AND)))
(declare-datatypes () ((Condition EQ NE CS CC MI PL VS VC HI LS GE LT GT LE AL)))
(declare-datatypes () ((Flag N S)))
(declare-datatypes () ((BarrelOp LSL LSR ASR)))
(declare-datatypes () ((Register R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 SP LR PC CPSR)))
(define-sort State () (Array Register (_ BitVec 32)))

(declare-var pre State)
(declare-var post State)

(declare-var oper Operation)
(declare-var cond Condition)
(declare-var flag Flag)
(declare-var rd Register)
(declare-var rn Register)
(declare-var rt Register)
(declare-var imm (_ BitVec 12))
(declare-var barrel_op BarrelOp)
(declare-var barrel_num (_ BitVec 32))

(declare-rel transition (State State))

(declare-rel transition_2 (State Operation Condition Flag Register Register State))
(declare-rel transition_2_barrel (State Operation Condition Flag Register Register BarrelOp (_ BitVec 32) State))
(declare-rel transition_3 (State Operation Condition Flag Register Register Register State))
(declare-rel transition_3_barrel (State Operation Condition Flag Register Register Register BarrelOp (_ BitVec 32) State))
(declare-rel transition_3_immediate (State Operation Condition Flag Register Register (_ BitVec 12) State))

(declare-rel eval_true (State Condition))
(declare-rel R0_to_LR_equal_except (State State Register))
(declare-rel R0_to_LR_equal (State State))
(declare-rel CPSR_equal (State State))
(declare-rel pc_incremented (State State))

(declare-rel N1 (State))
(declare-rel Z1 (State))
(declare-rel C1 (State))
(declare-rel V1 (State))

(declare-fun imm_rotate ((_ BitVec 12)) (_ BitVec 32))
(declare-fun barrel_shift (State Register BarrelOp (_ BitVec 32)) (_ BitVec 32))

(assert 
	(= 
		(barrel_shift pre rt LSL barrel_num) 
		(bvshl (select pre rt) barrel_num)
	)
)

(assert 
	(= 
		(barrel_shift pre rt LSR barrel_num) 
		(bvlshr (select pre rt) barrel_num)
	)
)

(assert 
	(= 
		(barrel_shift pre rt ASR barrel_num) 
		(bvashr (select pre rt) barrel_num)
	)
)

(assert (=> (R0_to_LR_equal_except pre post rd) 
	(and 
		(or (= (select pre R0) (select post R0)) (= rd R0))  
		(or (= (select pre R1) (select post R1)) (= rd R1))
		(or (= (select pre R2) (select post R2)) (= rd R2))
		(or (= (select pre R3) (select post R3)) (= rd R3))
		(or (= (select pre R4) (select post R4)) (= rd R4))
		(or (= (select pre R5) (select post R5)) (= rd R5))
		(or (= (select pre R6) (select post R6)) (= rd R6))
		(or (= (select pre R7) (select post R7)) (= rd R7))
		(or (= (select pre R8) (select post R8)) (= rd R8))
		(or (= (select pre R9) (select post R9)) (= rd R9))
		(or (= (select pre R10) (select post R10)) (= rd R10))
		(or (= (select pre R11) (select post R11)) (= rd R11))
		(or (= (select pre R12) (select post R12)) (= rd R12))
		(or (= (select pre SP) (select post SP)) (= rd SP))
		(or (= (select pre LR) (select post LR)) (= rd LR))
	)
))

(assert (=> (R0_to_LR_equal pre post) 
	(and 
		(= (select pre R0) (select post R0)) 
		(= (select pre R1) (select post R1))
		(= (select pre R2) (select post R2))
		(= (select pre R3) (select post R3))
		(= (select pre R4) (select post R4))
		(= (select pre R5) (select post R5))
		(= (select pre R6) (select post R6))
		(= (select pre R7) (select post R7))
		(= (select pre R8) (select post R8))
		(= (select pre R9) (select post R9))
		(= (select pre R10) (select post R10))
		(= (select pre R11) (select post R11))
		(= (select pre R12) (select post R12))
		(= (select pre SP) (select post SP))
		(= (select pre LR) (select post LR))
	)
))

(assert (=> (CPSR_equal pre post)
	(= (select pre CPSR) (select post CPSR))
))

(assert (=> (pc_incremented pre post)
	(= (select post PC) (bvadd (select pre PC) #b00000000000000000000000000000100))
))

(assert 
	;(and 
		;(= (bv2int ((_ extract 11 8) imm)) rotate_num) 
		(= 
			(imm_rotate imm) 
			((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm))) 
		)	
	;)
)

(assert (ite (N1 pre) (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 31 31) (select pre CPSR)) #b0)))
(assert (ite (N1 post) (= ((_ extract 31 31) (select post CPSR)) #b1) (= ((_ extract 31 31) (select post CPSR)) #b0)))
(assert (ite (Z1 pre) (= ((_ extract 30 30) (select pre CPSR)) #b1) (= ((_ extract 30 30) (select pre CPSR)) #b0)))
(assert (ite (Z1 post) (= ((_ extract 30 30) (select post CPSR)) #b1) (= ((_ extract 30 30) (select post CPSR)) #b0)))
(assert (ite (C1 pre) (= ((_ extract 29 29) (select pre CPSR)) #b1) (= ((_ extract 29 29) (select pre CPSR)) #b0)))
(assert (ite (C1 post) (= ((_ extract 29 29) (select post CPSR)) #b1) (= ((_ extract 29 29) (select post CPSR)) #b0)))
(assert (ite (V1 pre) (= ((_ extract 28 28) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b0)))
(assert (ite (V1 post) (= ((_ extract 28 28) (select post CPSR)) #b1) (= ((_ extract 28 28) (select post CPSR)) #b0)))

(assert (ite (eval_true pre EQ) (Z1 pre) (not (Z1 pre))))
(assert (ite (eval_true pre NE) (not (Z1 pre)) (Z1 pre)))
(assert (ite (eval_true pre CS) (C1 pre) (not (C1 pre)) ))
(assert (ite (eval_true pre CC) (not (C1 pre))  (C1 pre)))
(assert (ite (eval_true pre MI) (N1 pre) (not (N1 pre)) ))
(assert (ite (eval_true pre PL) (not (N1 pre)) (N1 pre) ))
(assert (ite (eval_true pre VS) (V1 pre) (not (V1 pre)) ))
(assert (ite (eval_true pre VC) (not (V1 pre)) (V1 pre) ))
(assert (ite (eval_true pre HI) (and (C1 pre) (not (Z1 pre))) (or (not (C1 pre)) (Z1 pre))))
(assert (ite (eval_true pre LS) (or (not (C1 pre)) (Z1 pre)) (and (C1 pre) (not (Z1 pre))) ))
(assert (ite (eval_true pre GE) (= (N1 pre) (V1 pre)) (not (= (N1 pre) (V1 pre))) ))
(assert (ite (eval_true pre LT) (not (= (N1 pre) (V1 pre))) (= (N1 pre) (V1 pre)) ))
(assert (ite (eval_true pre GT) (and (not (Z1 pre)) (= (N1 pre) (V1 pre))) (or (Z1 pre) (not (= (N1 pre) (V1 pre)))) ))
(assert (ite (eval_true pre LE) (or (Z1 pre) (not (= (N1 pre) (V1 pre)))) (and (not (Z1 pre)) (= (N1 pre) (V1 pre)))))
(assert (ite (eval_true pre AL) true false))

(assert (=> (transition_3 pre ADD cond flag rd rn rt post) 
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (select post rd) (bvadd (select pre rn) (select pre rt))) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)	 
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post)) 
	)
))

(assert (=> (transition_3_barrel pre ADD cond flag rd rn rt barrel_op barrel_num post) 
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (select post rd) (bvadd (select pre rn) (barrel_shift pre rt barrel_op barrel_num))) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)	 
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post)) 
	)
))

(assert (=> (transition_3_immediate pre ADD cond flag rd rn imm post) 
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (select post rd) (bvadd (select pre rn) (imm_rotate imm) )) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)	 
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post)) 
	)
))

(assert (=> (transition_3 pre SUB cond flag rd rn rt post) 
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (select post rd) (bvsub (select pre rn) (select pre rt))) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)	 
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post)) 
	)
))

(assert (=> (transition_3_barrel pre SUB cond flag rd rn rt barrel_op barrel_num post) 
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (select post rd) (bvsub (select pre rn) (barrel_shift pre rt barrel_op barrel_num))) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)	 
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post)) 
	)
))

(assert (=> (transition_3_immediate pre SUB cond flag rd rn imm post) 
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (select post rd) (bvsub (select pre rn) (imm_rotate imm) )) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)	 
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post)) 
	)
))

(assert (=> (transition_2 pre MOV cond flag rd rt post)
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (select pre rt) (select post rd)) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
	)
))

(assert (=> (transition_2_barrel pre MOV cond flag rd rt barrel_op barrel_num post)
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (barrel_shift pre rt barrel_op barrel_num) (select post rd)) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
	)
))

(assert (=> (transition_2 pre MVN cond flag rd rt post)
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (bvnot (select pre rt)) (select post rd)) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
	)
))

(assert (=> (transition_2_barrel pre MVN cond flag rd rt barrel_op barrel_num post)
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (bvnot (barrel_shift pre rt barrel_op barrel_num)) (select post rd)) (pc_incremented pre post)
			(or 
				(and (= flag N) (CPSR_equal pre post)) 
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post)) ))
			)
		)
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
	)
))

(assert (=> (transition_2 pre CMP cond flag rd rt post) 
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal pre post) (ite (= (select pre rd) (select pre rt)) (Z1 post) (not (Z1 post))) (pc_incremented pre post)) 
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
	)
))

(assert (=> (transition_2_barrel pre CMP cond flag rd rt barrel_op barrel_num post) 
	(or 
		(and (eval_true pre cond) (R0_to_LR_equal pre post) (ite (= (select pre rd) (barrel_shift pre rt barrel_op barrel_num)) (Z1 post) (not (Z1 post))) (pc_incremented pre post)) 
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
	)
))

(assert (=> (transition_3 pre AND cond flag rd rn rt post)
	(or
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (select post rd) (bvand (select pre rn) (select pre rt))) (pc_incremented pre post)
			(or
				(and (= flag N) (CPSR_equal pre post))
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post))))
			)
		)
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
	)
))

(assert (=> (transition_3_barrel pre AND cond flag rd rn rt barrel_op barrel_num post)
	(or
		(and (eval_true pre cond) (R0_to_LR_equal_except pre post rd) (= (select post rd) (bvand (select pre rn) (barrel_shift pre rt barrel_op barrel_num))) (pc_incremented pre post)
			(or
				(and (= flag N) (CPSR_equal pre post))
				(and (= flag S) (ite (= (select post rd) #b00000000000000000000000000000000) (Z1 post) (not (Z1 post))))
			)
		)
		(and (not (eval_true pre cond)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
	)
))

(assert (=> (transition_2 pre ADD cond flag rd rt post) false))
(assert (=> (transition_2_barrel pre ADD cond flag rd rt barrel_op barrel_num post) false))
;(assert (=> (transition_3_immediate pre ADD cond flag rd rn imm post) false))

(assert (=> (transition_2 pre SUB cond flag rd rt post) false))
(assert (=> (transition_2_barrel pre SUB cond flag rd rt barrel_op barrel_num post) false))
;(assert (=> (transition_3_immediate pre SUB cond flag rd rn imm post) false))

(assert (=> (transition_3 pre MOV cond flag rd rn rt post) false))
(assert (=> (transition_3_barrel pre MOV cond flag rd rn rt barrel_op barrel_num post) false))
(assert (=> (transition_3_immediate pre MOV cond flag rd rn imm post) false))

(assert (=> (transition_3 pre MVN cond flag rd rn rt post) false))
(assert (=> (transition_3_barrel pre MVN cond flag rd rn rt barrel_op barrel_num post) false))
(assert (=> (transition_3_immediate pre MVN cond flag rd rn imm post) false))

(assert (=> (transition_3 pre CMP cond flag rd rn rt post) false))
(assert (=> (transition_3_barrel pre CMP cond flag rd rn rt barrel_op barrel_num post) false))
(assert (=> (transition_3_immediate pre CMP cond flag rd rn imm post) false))

(assert (=> (transition_2 pre AND cond flag rd rt post) false))
(assert (=> (transition_2_barrel pre AND cond flag rd rt barrel_op barrel_num post) false))
(assert (=> (transition_3_immediate pre AND cond flag rd rn imm post) false))


(assert (=> (transition pre post)
	(or 
		(transition_2 pre oper cond flag rd rt post)
		(transition_2_barrel pre oper cond flag rd rt barrel_op barrel_num post)

		(transition_3 pre oper cond flag rd rn rt post)
		(transition_3_barrel pre oper cond flag rd rn rt barrel_op barrel_num post)

		(transition_3_immediate pre oper cond flag rd rn imm post)
	)
))

(assert (transition pre post))

(assert (= (select pre R0) #b00000000000000000000000000000000))
(assert (= (select pre R1) #b00000000000000000000000000001111))
(assert (= (select pre R2) #b00000000000000000000000000000000))
(assert (= (select pre R3) #b00000000000000000000000000000000))
(assert (= (select pre R4) #b00000000000000000000000000000000))
(assert (= (select pre R5) #b00000000000000000000000000000000))
(assert (= (select pre R6) #b00000000000000000000000000000000))
(assert (= (select pre R7) #b00000000000000000000000000000000))
(assert (= (select pre R8) #b00000000000000000000000000000000))
(assert (= (select pre R9) #b00000000000000000000000000000000))
(assert (= (select pre R10) #b00000000000000000000000000000000))
(assert (= (select pre R11) #b00000000000000000000000000000000))
(assert (= (select pre R12) #b00000000000000000000000000000000))
(assert (= (select pre SP) #b00000000000000000000000000000000))
(assert (= (select pre LR) #b00000000000000000000000000000000))
(assert (= (select pre PC) #b00000000000000000000000000000000))
(assert (= (select pre CPSR) #b01000000000000000000000000000000))

(assert (= (select post R0) #b00000000100000000000000000001111))
(assert (= (select post R1) #b00000000000000000000000000001111))
(assert (= (select post R2) #b00000000000000000000000000000000))
(assert (= (select post R3) #b00000000000000000000000000000000))
(assert (= (select post R4) #b00000000000000000000000000000000))
(assert (= (select post R5) #b00000000000000000000000000000000))
(assert (= (select post R6) #b00000000000000000000000000000000))
(assert (= (select post R7) #b00000000000000000000000000000000))
(assert (= (select post R8) #b00000000000000000000000000000000))
(assert (= (select post R9) #b00000000000000000000000000000000))
(assert (= (select post R10) #b00000000000000000000000000000000))
(assert (= (select post R11) #b00000000000000000000000000000000))
(assert (= (select post R12) #b00000000000000000000000000000000))
(assert (= (select post SP) #b00000000000000000000000000000000))
(assert (= (select post LR) #b00000000000000000000000000000000))
(assert (= (select post PC) #b00000000000000000000000000000100))
(assert (= (select post CPSR) #b01000000000000000000000000000000))

;(assert (= flag S))

(check-sat)

(get-model)

; TODO
; more flexible templates for operations
; immediate value shifting/rotating
; instructions with barrel shifter
; negative, carry, overflow flags setting
; more readable output, says what format the instruction is: 2/3/imm args etc




















