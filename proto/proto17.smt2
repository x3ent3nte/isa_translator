(echo "Generating ARM contraints")

(declare-datatypes () ((Operation ADD SUB MOV MVN CMP AND)))
(declare-datatypes () ((Condition EQ NE CS CC MI PL VS VC HI LS GE LT GT LE AL)))
(declare-datatypes () ((Flag N S)))
(declare-datatypes () ((BarrelOp LSL LSR ASR)))
(declare-datatypes () ((Register R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 SP LR PC CPSR)))
(define-sort State () (Array Register (_ BitVec 32)))

(declare-rel transition (State State))

; Variables
(declare-var pre State)
(declare-var mid State)
(declare-var mid_two State)
(declare-var mid_three State)
(declare-var mid_four State)
(declare-var mid_five State)
(declare-var post State)

(declare-var oper Operation)
(declare-var cond Condition)
(declare-var flag Flag)
(declare-var rd Register)
(declare-var rn Register)
(declare-var ro Register)

(declare-var imm (_ BitVec 12))
(declare-var imm_used Bool)

(declare-var barrel_op BarrelOp)
(declare-var barrel_num (_ BitVec 32))

(declare-var oper2 Operation)
(declare-var cond2 Condition)
(declare-var flag2 Flag)
(declare-var rd2 Register)
(declare-var rn2 Register)
(declare-var ro2 Register)

(declare-var imm2 (_ BitVec 12))
(declare-var imm_used2 Bool)

(declare-var barrel_op2 BarrelOp)
(declare-var barrel_num2 (_ BitVec 32))

(declare-var oper3 Operation)
(declare-var cond3 Condition)
(declare-var flag3 Flag)
(declare-var rd3 Register)
(declare-var rn3 Register)
(declare-var ro3 Register)

(declare-var imm3 (_ BitVec 12))
(declare-var imm_used3 Bool)

(declare-var barrel_op3 BarrelOp)
(declare-var barrel_num3 (_ BitVec 32))

(declare-var oper4 Operation)
(declare-var cond4 Condition)
(declare-var flag4 Flag)
(declare-var rd4 Register)
(declare-var rn4 Register)
(declare-var ro4 Register)

(declare-var imm4 (_ BitVec 12))
(declare-var imm_used4 Bool)

(declare-var barrel_op4 BarrelOp)
(declare-var barrel_num4 (_ BitVec 32))

(declare-var oper5 Operation)
(declare-var cond5 Condition)
(declare-var flag5 Flag)
(declare-var rd5 Register)
(declare-var rn5 Register)
(declare-var ro5 Register)

(declare-var imm5 (_ BitVec 12))
(declare-var imm_used5 Bool)

(declare-var barrel_op5 BarrelOp)
(declare-var barrel_num5 (_ BitVec 32))

(declare-var oper6 Operation)
(declare-var cond6 Condition)
(declare-var flag6 Flag)
(declare-var rd6 Register)
(declare-var rn6 Register)
(declare-var ro6 Register)

(declare-var imm6 (_ BitVec 12))
(declare-var imm_used6 Bool)

(declare-var barrel_op6 BarrelOp)
(declare-var barrel_num6 (_ BitVec 32))

(assert (=> (transition pre mid)
	(and
		; pc incremented
		( = (select mid PC) (bvadd (select pre PC) #b00000000000000000000000000000100))
		(or 
			; instruction not executed
			(and 
				; condition is false
				(not
					(or
						(and (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1))
						(and (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0))
						(and (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1))
						(and (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0))
						(and (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1))
						(and (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0))
						(and (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1))
						(and (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0))

						(and (= cond HI) (and (= ((_ extract 29 29) (select pre CPSR)) #b1) (= ((_ extract 30 30) (select pre CPSR)) #b0) ))
						(and (= cond LS) (or (= ((_ extract 29 29) (select pre CPSR)) #b0) (= ((_ extract 30 30) (select pre CPSR)) #b1) ))
						(and (= cond GE) (and (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1) ) )
						(and (= cond LT) (or (= ((_ extract 31 31) (select pre CPSR)) #b0) (= ((_ extract 28 28) (select pre CPSR)) #b0) ) )
						(and 
							(= cond GT) 
							(and 
								(= ((_ extract 30 30) (select pre CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)) 
							)

						)
						(and 
							(= cond LE) 
							(or 
								(= ((_ extract 30 30) (select pre CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))) )
						)
						(= cond AL)
					)
				) 
				; R0 to CPSR are equal
				(= (select pre R0) (select mid R0)) 
				(= (select pre R1) (select mid R1))
				(= (select pre R2) (select mid R2))
				(= (select pre R3) (select mid R3))
				(= (select pre R4) (select mid R4))
				(= (select pre R5) (select mid R5))
				(= (select pre R6) (select mid R6))
				(= (select pre R7) (select mid R7))
				(= (select pre R8) (select mid R8))
				(= (select pre R9) (select mid R9))
				(= (select pre R10) (select mid R10))
				(= (select pre R11) (select mid R11))
				(= (select pre R12) (select mid R12))
				(= (select pre SP) (select mid SP))
				(= (select pre LR) (select mid LR)) 
				(= (select pre CPSR) (select mid CPSR)) 
			)

			(let 
				((flex_val 
					(let ((val_to_shift (ite imm_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm))) (select pre ro))))
						(ite
							(= barrel_op LSL)
							(bvshl val_to_shift barrel_num)
							(ite 
								(= barrel_op LSR)
								(bvlshr val_to_shift barrel_num)
								(bvashr val_to_shift barrel_num)
							)

						)
					)
				)
				(rd_val (select mid rd)) (rn_val (select pre rn))
				)

				; instruction is executed
				(and
					; condition is true
					(or
						(and (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1))
						(and (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0))
						(and (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1))
						(and (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0))
						(and (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1))
						(and (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0))
						(and (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1))
						(and (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0))

						(and (= cond HI) (and (= ((_ extract 29 29) (select pre CPSR)) #b1) (= ((_ extract 30 30) (select pre CPSR)) #b0) ))
						(and (= cond LS) (or (= ((_ extract 29 29) (select pre CPSR)) #b0) (= ((_ extract 30 30) (select pre CPSR)) #b1) ))
						(and (= cond GE) (and (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1) ) )
						(and (= cond LT) (or (= ((_ extract 31 31) (select pre CPSR)) #b0) (= ((_ extract 28 28) (select pre CPSR)) #b0) ) )
						(and 
							(= cond GT) 
							(and 
								(= ((_ extract 30 30) (select pre CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)) 
							)

						)
						(and 
							(= cond LE) 
							(or 
								(= ((_ extract 30 30) (select pre CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))) )
						)
						(= cond AL)
					)
					(or
						; execute_compare
						(and
							(or (= oper CMP)) 

							; R0 to LR are equal
							(= (select pre R0) (select mid R0)) 
							(= (select pre R1) (select mid R1))
							(= (select pre R2) (select mid R2))
							(= (select pre R3) (select mid R3))
							(= (select pre R4) (select mid R4))
							(= (select pre R5) (select mid R5))
							(= (select pre R6) (select mid R6))
							(= (select pre R7) (select mid R7))
							(= (select pre R8) (select mid R8))
							(= (select pre R9) (select mid R9))
							(= (select pre R10) (select mid R10))
							(= (select pre R11) (select mid R11))
							(= (select pre R12) (select mid R12))
							(= (select pre SP) (select mid SP))
							(= (select pre LR) (select mid LR))

							(or (not (= oper CMP)) 
								(ite (= rn_val flex_val)
									(= ((_ extract 30 30) (select mid CPSR)) #b1) ; Z flag is 1
									(= ((_ extract 30 30) (select mid CPSR)) #b0) ; Z flag is 0
								)
							)			
						)
						
						(and
							; R0 to LR equal except destination register
							(or (= (select pre R0) (select mid R0)) (= rd R0))  
							(or (= (select pre R1) (select mid R1)) (= rd R1))
							(or (= (select pre R2) (select mid R2)) (= rd R2))
							(or (= (select pre R3) (select mid R3)) (= rd R3))
							(or (= (select pre R4) (select mid R4)) (= rd R4))
							(or (= (select pre R5) (select mid R5)) (= rd R5))
							(or (= (select pre R6) (select mid R6)) (= rd R6))
							(or (= (select pre R7) (select mid R7)) (= rd R7))
							(or (= (select pre R8) (select mid R8)) (= rd R8))
							(or (= (select pre R9) (select mid R9)) (= rd R9))
							(or (= (select pre R10) (select mid R10)) (= rd R10))
							(or (= (select pre R11) (select mid R11)) (= rd R11))
							(or (= (select pre R12) (select mid R12)) (= rd R12))
							(or (= (select pre SP) (select mid SP)) (= rd SP))
							(or (= (select pre LR) (select mid LR)) (= rd LR))

							(or
								; execute_2
								(and
									(or (= oper MOV) (= oper MVN))

									(or (not (= oper MOV)) (= rd_val flex_val))
									(or (not (= oper MVN)) (= rd_val (bvnot flex_val)))
								)
								; execute_3
								(and
									(or (= oper ADD) (= oper SUB) (= oper AND)) 

									(or (not (= oper ADD)) (= rd_val (bvadd rn_val flex_val)))
									(or (not (= oper SUB)) (= rd_val (bvsub rn_val flex_val)))
									(or (not (= oper AND)) (= rd_val (bvand rn_val flex_val)))
								)
							)
							; set flags or not
							(or 
								(and (= flag N) (= (select pre CPSR) (select mid CPSR))) 
								(and (= flag S) 
									(ite 
										(= (select mid rd) #b00000000000000000000000000000000) 
										(= ((_ extract 30 30) (select mid CPSR)) #b1) ;Z flag is 1
										(= ((_ extract 30 30) (select mid CPSR)) #b0) ;Z flag is 0
									)
								)
							)
						)
					)
				)
			)
		)
	)
))

(assert (=> (transition mid mid_two)
	(and
		; pc incremented
		( = (select mid_two PC) (bvadd (select mid PC) #b00000000000000000000000000000100))
		(or 
			; instruction not executed
			(and 
				; cond2ition is false
				(not
					(or
						(and (= cond2 EQ) (= ((_ extract 30 30) (select mid CPSR)) #b1))
						(and (= cond2 NE) (= ((_ extract 30 30) (select mid CPSR)) #b0))
						(and (= cond2 CS) (= ((_ extract 29 29) (select mid CPSR)) #b1))
						(and (= cond2 CC) (= ((_ extract 29 29) (select mid CPSR)) #b0))
						(and (= cond2 MI) (= ((_ extract 31 31) (select mid CPSR)) #b1))
						(and (= cond2 PL) (= ((_ extract 31 31) (select mid CPSR)) #b0))
						(and (= cond2 VS) (= ((_ extract 28 28) (select mid CPSR)) #b1))
						(and (= cond2 VC) (= ((_ extract 28 28) (select mid CPSR)) #b0))

						(and (= cond2 HI) (and (= ((_ extract 29 29) (select mid CPSR)) #b1) (= ((_ extract 30 30) (select mid CPSR)) #b0) ))
						(and (= cond2 LS) (or (= ((_ extract 29 29) (select mid CPSR)) #b0) (= ((_ extract 30 30) (select mid CPSR)) #b1) ))
						(and (= cond2 GE) (and (= ((_ extract 31 31) (select mid CPSR)) #b1) (= ((_ extract 28 28) (select mid CPSR)) #b1) ) )
						(and (= cond2 LT) (or (= ((_ extract 31 31) (select mid CPSR)) #b0) (= ((_ extract 28 28) (select mid CPSR)) #b0) ) )
						(and 
							(= cond2 GT) 
							(and 
								(= ((_ extract 30 30) (select mid CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid CPSR)) #b1) (= ((_ extract 28 28) (select mid CPSR)) #b1)) 
							)

						)
						(and 
							(= cond2 LE) 
							(or 
								(= ((_ extract 30 30) (select mid CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid CPSR)) #b1) (= ((_ extract 28 28) (select mid CPSR)) #b1))) )
						)
						(= cond2 AL)
					)
				) 
				; R0 to CPSR are equal
				(= (select mid R0) (select mid_two R0)) 
				(= (select mid R1) (select mid_two R1))
				(= (select mid R2) (select mid_two R2))
				(= (select mid R3) (select mid_two R3))
				(= (select mid R4) (select mid_two R4))
				(= (select mid R5) (select mid_two R5))
				(= (select mid R6) (select mid_two R6))
				(= (select mid R7) (select mid_two R7))
				(= (select mid R8) (select mid_two R8))
				(= (select mid R9) (select mid_two R9))
				(= (select mid R10) (select mid_two R10))
				(= (select mid R11) (select mid_two R11))
				(= (select mid R12) (select mid_two R12))
				(= (select mid SP) (select mid_two SP))
				(= (select mid LR) (select mid_two LR)) 
				(= (select mid CPSR) (select mid_two CPSR)) 
			)

			(let 
				((flex_val 
					(let ((val_to_shift (ite imm_used2 ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm2))) (select mid ro2))))
						(ite
							(= barrel_op2 LSL)
							(bvshl val_to_shift barrel_num2)
							(ite 
								(= barrel_op2 LSR)
								(bvlshr val_to_shift barrel_num2)
								(bvashr val_to_shift barrel_num2)
							)

						)
					)
				)
				(rd2_val (select mid_two rd2 )) (rn2_val (select mid rn2 ))
				)

				; instruction is executed
				(and
					; cond2ition is true
					(or
						(and (= cond2 EQ) (= ((_ extract 30 30) (select mid CPSR)) #b1))
						(and (= cond2 NE) (= ((_ extract 30 30) (select mid CPSR)) #b0))
						(and (= cond2 CS) (= ((_ extract 29 29) (select mid CPSR)) #b1))
						(and (= cond2 CC) (= ((_ extract 29 29) (select mid CPSR)) #b0))
						(and (= cond2 MI) (= ((_ extract 31 31) (select mid CPSR)) #b1))
						(and (= cond2 PL) (= ((_ extract 31 31) (select mid CPSR)) #b0))
						(and (= cond2 VS) (= ((_ extract 28 28) (select mid CPSR)) #b1))
						(and (= cond2 VC) (= ((_ extract 28 28) (select mid CPSR)) #b0))

						(and (= cond2 HI) (and (= ((_ extract 29 29) (select mid CPSR)) #b1) (= ((_ extract 30 30) (select mid CPSR)) #b0) ))
						(and (= cond2 LS) (or (= ((_ extract 29 29) (select mid CPSR)) #b0) (= ((_ extract 30 30) (select mid CPSR)) #b1) ))
						(and (= cond2 GE) (and (= ((_ extract 31 31) (select mid CPSR)) #b1) (= ((_ extract 28 28) (select mid CPSR)) #b1) ) )
						(and (= cond2 LT) (or (= ((_ extract 31 31) (select mid CPSR)) #b0) (= ((_ extract 28 28) (select mid CPSR)) #b0) ) )
						(and 
							(= cond2 GT) 
							(and 
								(= ((_ extract 30 30) (select mid CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid CPSR)) #b1) (= ((_ extract 28 28) (select mid CPSR)) #b1)) 
							)

						)
						(and 
							(= cond2 LE) 
							(or 
								(= ((_ extract 30 30) (select mid CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid CPSR)) #b1) (= ((_ extract 28 28) (select mid CPSR)) #b1))) )
						)
						(= cond2 AL)
					)
					(or
						; execute_compare
						(and
							(or (= oper2 CMP)) 

							; R0 to LR are equal
							(= (select mid R0) (select mid_two R0)) 
							(= (select mid R1) (select mid_two R1))
							(= (select mid R2) (select mid_two R2))
							(= (select mid R3) (select mid_two R3))
							(= (select mid R4) (select mid_two R4))
							(= (select mid R5) (select mid_two R5))
							(= (select mid R6) (select mid_two R6))
							(= (select mid R7) (select mid_two R7))
							(= (select mid R8) (select mid_two R8))
							(= (select mid R9) (select mid_two R9))
							(= (select mid R10) (select mid_two R10))
							(= (select mid R11) (select mid_two R11))
							(= (select mid R12) (select mid_two R12))
							(= (select mid SP) (select mid_two SP))
							(= (select mid LR) (select mid_two LR))

							(or (not (= oper2 CMP)) 
								(ite (= rn2_val flex_val)
									(= ((_ extract 30 30) (select mid_two CPSR)) #b1) ; Z flag2 is 1
									(= ((_ extract 30 30) (select mid_two CPSR)) #b0) ; Z flag2 is 0
								)
							)			
						)
						
						(and
							; R0 to LR equal except destination register
							(or (= (select mid R0) (select mid_two R0)) (= rd2 R0))  
							(or (= (select mid R1) (select mid_two R1)) (= rd2 R1))
							(or (= (select mid R2) (select mid_two R2)) (= rd2 R2))
							(or (= (select mid R3) (select mid_two R3)) (= rd2 R3))
							(or (= (select mid R4) (select mid_two R4)) (= rd2 R4))
							(or (= (select mid R5) (select mid_two R5)) (= rd2 R5))
							(or (= (select mid R6) (select mid_two R6)) (= rd2 R6))
							(or (= (select mid R7) (select mid_two R7)) (= rd2 R7))
							(or (= (select mid R8) (select mid_two R8)) (= rd2 R8))
							(or (= (select mid R9) (select mid_two R9)) (= rd2 R9))
							(or (= (select mid R10) (select mid_two R10)) (= rd2 R10))
							(or (= (select mid R11) (select mid_two R11)) (= rd2 R11))
							(or (= (select mid R12) (select mid_two R12)) (= rd2 R12))
							(or (= (select mid SP) (select mid_two SP)) (= rd2 SP))
							(or (= (select mid LR) (select mid_two LR)) (= rd2 LR))

							(or
								; execute_2
								(and
									(or (= oper2 MOV) (= oper2 MVN))

									(or (not (= oper2 MOV)) (= rd2_val flex_val))
									(or (not (= oper2 MVN)) (= rd2_val (bvnot flex_val)))
								)
								; execute_3
								(and
									(or (= oper2 ADD) (= oper2 SUB) (= oper2 AND)) 

									(or (not (= oper2 ADD)) (= rd2_val (bvadd rn2_val flex_val)))
									(or (not (= oper2 SUB)) (= rd2_val (bvsub rn2_val flex_val)))
									(or (not (= oper2 AND)) (= rd2_val (bvand rn2_val flex_val)))
								)
							)
							; set flag2s or not
							(or 
								(and (= flag2 N) (= (select mid CPSR) (select mid_two CPSR))) 
								(and (= flag2 S) 
									(ite 
										(= (select mid_two rd2 ) #b00000000000000000000000000000000) 
										(= ((_ extract 30 30) (select mid_two CPSR)) #b1) ;Z flag2 is 1
										(= ((_ extract 30 30) (select mid_two CPSR)) #b0) ;Z flag2 is 0
									)
								)
							)
						)
					)
				)
			)
		)
	)
))

(assert (=> (transition mid_two mid_three)
	(and
		; pc incremented
		( = (select mid_three PC) (bvadd (select mid_two PC) #b00000000000000000000000000000100))
		(or 
			; instruction not executed
			(and 
				; condition is false
				(not
					(or
						(and (= cond3 EQ) (= ((_ extract 30 30) (select mid_two CPSR)) #b1))
						(and (= cond3 NE) (= ((_ extract 30 30) (select mid_two CPSR)) #b0))
						(and (= cond3 CS) (= ((_ extract 29 29) (select mid_two CPSR)) #b1))
						(and (= cond3 CC) (= ((_ extract 29 29) (select mid_two CPSR)) #b0))
						(and (= cond3 MI) (= ((_ extract 31 31) (select mid_two CPSR)) #b1))
						(and (= cond3 PL) (= ((_ extract 31 31) (select mid_two CPSR)) #b0))
						(and (= cond3 VS) (= ((_ extract 28 28) (select mid_two CPSR)) #b1))
						(and (= cond3 VC) (= ((_ extract 28 28) (select mid_two CPSR)) #b0))

						(and (= cond3 HI) (and (= ((_ extract 29 29) (select mid_two CPSR)) #b1) (= ((_ extract 30 30) (select mid_two CPSR)) #b0) ))
						(and (= cond3 LS) (or (= ((_ extract 29 29) (select mid_two CPSR)) #b0) (= ((_ extract 30 30) (select mid_two CPSR)) #b1) ))
						(and (= cond3 GE) (and (= ((_ extract 31 31) (select mid_two CPSR)) #b1) (= ((_ extract 28 28) (select mid_two CPSR)) #b1) ) )
						(and (= cond3 LT) (or (= ((_ extract 31 31) (select mid_two CPSR)) #b0) (= ((_ extract 28 28) (select mid_two CPSR)) #b0) ) )
						(and 
							(= cond3 GT) 
							(and 
								(= ((_ extract 30 30) (select mid_two CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid_two CPSR)) #b1) (= ((_ extract 28 28) (select mid_two CPSR)) #b1)) 
							)

						)
						(and 
							(= cond3 LE) 
							(or 
								(= ((_ extract 30 30) (select mid_two CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid_two CPSR)) #b1) (= ((_ extract 28 28) (select mid_two CPSR)) #b1))) )
						)
						(= cond3 AL)
					)
				) 
				; R0 to CPSR are equal
				(= (select mid_two R0) (select mid_three R0)) 
				(= (select mid_two R1) (select mid_three R1))
				(= (select mid_two R2) (select mid_three R2))
				(= (select mid_two R3) (select mid_three R3))
				(= (select mid_two R4) (select mid_three R4))
				(= (select mid_two R5) (select mid_three R5))
				(= (select mid_two R6) (select mid_three R6))
				(= (select mid_two R7) (select mid_three R7))
				(= (select mid_two R8) (select mid_three R8))
				(= (select mid_two R9) (select mid_three R9))
				(= (select mid_two R10) (select mid_three R10))
				(= (select mid_two R11) (select mid_three R11))
				(= (select mid_two R12) (select mid_three R12))
				(= (select mid_two SP) (select mid_three SP))
				(= (select mid_two LR) (select mid_three LR)) 
				(= (select mid_two CPSR) (select mid_three CPSR)) 
			)

			(let 
				((flex_val 
					(let ((val_to_shift (ite imm_used3 ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm3))) (select mid_two ro3))))
						(ite
							(= barrel_op3 LSL)
							(bvshl val_to_shift barrel_num3)
							(ite 
								(= barrel_op3 LSR)
								(bvlshr val_to_shift barrel_num3)
								(bvashr val_to_shift barrel_num3)
							)

						)
					)
				)
				(rd3_val (select mid_three rd3 )) (rn3_val (select mid_two rn3 ))
				)

				; instruction is executed
				(and
					; cond3ition is true
					(or
						(and (= cond3 EQ) (= ((_ extract 30 30) (select mid_two CPSR)) #b1))
						(and (= cond3 NE) (= ((_ extract 30 30) (select mid_two CPSR)) #b0))
						(and (= cond3 CS) (= ((_ extract 29 29) (select mid_two CPSR)) #b1))
						(and (= cond3 CC) (= ((_ extract 29 29) (select mid_two CPSR)) #b0))
						(and (= cond3 MI) (= ((_ extract 31 31) (select mid_two CPSR)) #b1))
						(and (= cond3 PL) (= ((_ extract 31 31) (select mid_two CPSR)) #b0))
						(and (= cond3 VS) (= ((_ extract 28 28) (select mid_two CPSR)) #b1))
						(and (= cond3 VC) (= ((_ extract 28 28) (select mid_two CPSR)) #b0))

						(and (= cond3 HI) (and (= ((_ extract 29 29) (select mid_two CPSR)) #b1) (= ((_ extract 30 30) (select mid_two CPSR)) #b0) ))
						(and (= cond3 LS) (or (= ((_ extract 29 29) (select mid_two CPSR)) #b0) (= ((_ extract 30 30) (select mid_two CPSR)) #b1) ))
						(and (= cond3 GE) (and (= ((_ extract 31 31) (select mid_two CPSR)) #b1) (= ((_ extract 28 28) (select mid_two CPSR)) #b1) ) )
						(and (= cond3 LT) (or (= ((_ extract 31 31) (select mid_two CPSR)) #b0) (= ((_ extract 28 28) (select mid_two CPSR)) #b0) ) )
						(and 
							(= cond3 GT) 
							(and 
								(= ((_ extract 30 30) (select mid_two CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid_two CPSR)) #b1) (= ((_ extract 28 28) (select mid_two CPSR)) #b1)) 
							)

						)
						(and 
							(= cond3 LE) 
							(or 
								(= ((_ extract 30 30) (select mid_two CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid_two CPSR)) #b1) (= ((_ extract 28 28) (select mid_two CPSR)) #b1))) )
						)
						(= cond3 AL)
					)
					(or
						; execute_compare
						(and
							(or (= oper3 CMP)) 

							; R0 to LR are equal
							(= (select mid_two R0) (select mid_three R0)) 
							(= (select mid_two R1) (select mid_three R1))
							(= (select mid_two R2) (select mid_three R2))
							(= (select mid_two R3) (select mid_three R3))
							(= (select mid_two R4) (select mid_three R4))
							(= (select mid_two R5) (select mid_three R5))
							(= (select mid_two R6) (select mid_three R6))
							(= (select mid_two R7) (select mid_three R7))
							(= (select mid_two R8) (select mid_three R8))
							(= (select mid_two R9) (select mid_three R9))
							(= (select mid_two R10) (select mid_three R10))
							(= (select mid_two R11) (select mid_three R11))
							(= (select mid_two R12) (select mid_three R12))
							(= (select mid_two SP) (select mid_three SP))
							(= (select mid_two LR) (select mid_three LR))

							(or (not (= oper3 CMP)) 
								(ite (= rn3_val flex_val)
									(= ((_ extract 30 30) (select mid_three CPSR)) #b1) ; Z flag3 is 1
									(= ((_ extract 30 30) (select mid_three CPSR)) #b0) ; Z flag3 is 0
								)
							)			
						)
						
						(and
							; R0 to LR equal except destination register
							(or (= (select mid_two R0) (select mid_three R0)) (= rd3 R0))  
							(or (= (select mid_two R1) (select mid_three R1)) (= rd3 R1))
							(or (= (select mid_two R2) (select mid_three R2)) (= rd3 R2))
							(or (= (select mid_two R3) (select mid_three R3)) (= rd3 R3))
							(or (= (select mid_two R4) (select mid_three R4)) (= rd3 R4))
							(or (= (select mid_two R5) (select mid_three R5)) (= rd3 R5))
							(or (= (select mid_two R6) (select mid_three R6)) (= rd3 R6))
							(or (= (select mid_two R7) (select mid_three R7)) (= rd3 R7))
							(or (= (select mid_two R8) (select mid_three R8)) (= rd3 R8))
							(or (= (select mid_two R9) (select mid_three R9)) (= rd3 R9))
							(or (= (select mid_two R10) (select mid_three R10)) (= rd3 R10))
							(or (= (select mid_two R11) (select mid_three R11)) (= rd3 R11))
							(or (= (select mid_two R12) (select mid_three R12)) (= rd3 R12))
							(or (= (select mid_two SP) (select mid_three SP)) (= rd3 SP))
							(or (= (select mid_two LR) (select mid_three LR)) (= rd3 LR))

							(or
								; execute_2
								(and
									(or (= oper3 MOV) (= oper3 MVN))

									(or (not (= oper3 MOV)) (= rd3_val flex_val))
									(or (not (= oper3 MVN)) (= rd3_val (bvnot flex_val)))
								)
								; execute_3
								(and
									(or (= oper3 ADD) (= oper3 SUB) (= oper3 AND)) 

									(or (not (= oper3 ADD)) (= rd3_val (bvadd rn3_val flex_val)))
									(or (not (= oper3 SUB)) (= rd3_val (bvsub rn3_val flex_val)))
									(or (not (= oper3 AND)) (= rd3_val (bvand rn3_val flex_val)))
								)
							)
							; set flag3s or not
							(or 
								(and (= flag3 N) (= (select mid_two CPSR) (select mid_three CPSR))) 
								(and (= flag3 S) 
									(ite 
										(= (select mid_three rd3 ) #b00000000000000000000000000000000) 
										(= ((_ extract 30 30) (select mid_three CPSR)) #b1) ;Z flag3 is 1
										(= ((_ extract 30 30) (select mid_three CPSR)) #b0) ;Z flag3 is 0
									)
								)
							)
						)
					)
				)
			)
		)
	)
))

(assert (=> (transition mid_three mid_four)
	(and
		; pc incremented
		( = (select mid_four PC) (bvadd (select mid_three PC) #b00000000000000000000000000000100))
		(or 
			; instruction not executed
			(and 
				; cond3ition is false
				(not
					(or
						(and (= cond4 EQ) (= ((_ extract 30 30) (select mid_three CPSR)) #b1))
						(and (= cond4 NE) (= ((_ extract 30 30) (select mid_three CPSR)) #b0))
						(and (= cond4 CS) (= ((_ extract 29 29) (select mid_three CPSR)) #b1))
						(and (= cond4 CC) (= ((_ extract 29 29) (select mid_three CPSR)) #b0))
						(and (= cond4 MI) (= ((_ extract 31 31) (select mid_three CPSR)) #b1))
						(and (= cond4 PL) (= ((_ extract 31 31) (select mid_three CPSR)) #b0))
						(and (= cond4 VS) (= ((_ extract 28 28) (select mid_three CPSR)) #b1))
						(and (= cond4 VC) (= ((_ extract 28 28) (select mid_three CPSR)) #b0))

						(and (= cond4 HI) (and (= ((_ extract 29 29) (select mid_three CPSR)) #b1) (= ((_ extract 30 30) (select mid_three CPSR)) #b0) ))
						(and (= cond4 LS) (or (= ((_ extract 29 29) (select mid_three CPSR)) #b0) (= ((_ extract 30 30) (select mid_three CPSR)) #b1) ))
						(and (= cond4 GE) (and (= ((_ extract 31 31) (select mid_three CPSR)) #b1) (= ((_ extract 28 28) (select mid_three CPSR)) #b1) ) )
						(and (= cond4 LT) (or (= ((_ extract 31 31) (select mid_three CPSR)) #b0) (= ((_ extract 28 28) (select mid_three CPSR)) #b0) ) )
						(and 
							(= cond4 GT) 
							(and 
								(= ((_ extract 30 30) (select mid_three CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid_three CPSR)) #b1) (= ((_ extract 28 28) (select mid_three CPSR)) #b1)) 
							)

						)
						(and 
							(= cond4 LE) 
							(or 
								(= ((_ extract 30 30) (select mid_three CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid_three CPSR)) #b1) (= ((_ extract 28 28) (select mid_three CPSR)) #b1))) )
						)
						(= cond4 AL)
					)
				) 
				; R0 to CPSR are equal
				(= (select mid_three R0) (select mid_four R0)) 
				(= (select mid_three R1) (select mid_four R1))
				(= (select mid_three R2) (select mid_four R2))
				(= (select mid_three R3) (select mid_four R3))
				(= (select mid_three R4) (select mid_four R4))
				(= (select mid_three R5) (select mid_four R5))
				(= (select mid_three R6) (select mid_four R6))
				(= (select mid_three R7) (select mid_four R7))
				(= (select mid_three R8) (select mid_four R8))
				(= (select mid_three R9) (select mid_four R9))
				(= (select mid_three R10) (select mid_four R10))
				(= (select mid_three R11) (select mid_four R11))
				(= (select mid_three R12) (select mid_four R12))
				(= (select mid_three SP) (select mid_four SP))
				(= (select mid_three LR) (select mid_four LR)) 
				(= (select mid_three CPSR) (select mid_four CPSR)) 
			)

			(let 
				((flex_val 
					(let ((val_to_shift (ite imm_used4 ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm4))) (select mid_three ro4))))
						(ite
							(= barrel_op4 LSL)
							(bvshl val_to_shift barrel_num4)
							(ite 
								(= barrel_op4 LSR)
								(bvlshr val_to_shift barrel_num4)
								(bvashr val_to_shift barrel_num4)
							)

						)
					)
				)
				(rd4_val (select mid_four rd4 )) (rn4_val (select mid_three rn4 ))
				)

				; instruction is executed
				(and
					; cond3ition is true
					(or
						(and (= cond4 EQ) (= ((_ extract 30 30) (select mid_three CPSR)) #b1))
						(and (= cond4 NE) (= ((_ extract 30 30) (select mid_three CPSR)) #b0))
						(and (= cond4 CS) (= ((_ extract 29 29) (select mid_three CPSR)) #b1))
						(and (= cond4 CC) (= ((_ extract 29 29) (select mid_three CPSR)) #b0))
						(and (= cond4 MI) (= ((_ extract 31 31) (select mid_three CPSR)) #b1))
						(and (= cond4 PL) (= ((_ extract 31 31) (select mid_three CPSR)) #b0))
						(and (= cond4 VS) (= ((_ extract 28 28) (select mid_three CPSR)) #b1))
						(and (= cond4 VC) (= ((_ extract 28 28) (select mid_three CPSR)) #b0))

						(and (= cond4 HI) (and (= ((_ extract 29 29) (select mid_three CPSR)) #b1) (= ((_ extract 30 30) (select mid_three CPSR)) #b0) ))
						(and (= cond4 LS) (or (= ((_ extract 29 29) (select mid_three CPSR)) #b0) (= ((_ extract 30 30) (select mid_three CPSR)) #b1) ))
						(and (= cond4 GE) (and (= ((_ extract 31 31) (select mid_three CPSR)) #b1) (= ((_ extract 28 28) (select mid_three CPSR)) #b1) ) )
						(and (= cond4 LT) (or (= ((_ extract 31 31) (select mid_three CPSR)) #b0) (= ((_ extract 28 28) (select mid_three CPSR)) #b0) ) )
						(and 
							(= cond4 GT) 
							(and 
								(= ((_ extract 30 30) (select mid_three CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid_three CPSR)) #b1) (= ((_ extract 28 28) (select mid_three CPSR)) #b1)) 
							)

						)
						(and 
							(= cond4 LE) 
							(or 
								(= ((_ extract 30 30) (select mid_three CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid_three CPSR)) #b1) (= ((_ extract 28 28) (select mid_three CPSR)) #b1))) )
						)
						(= cond4 AL)
					)
					(or
						; execute_compare
						(and
							(or (= oper4 CMP)) 

							; R0 to LR are equal
							(= (select mid_three R0) (select mid_four R0)) 
							(= (select mid_three R1) (select mid_four R1))
							(= (select mid_three R2) (select mid_four R2))
							(= (select mid_three R3) (select mid_four R3))
							(= (select mid_three R4) (select mid_four R4))
							(= (select mid_three R5) (select mid_four R5))
							(= (select mid_three R6) (select mid_four R6))
							(= (select mid_three R7) (select mid_four R7))
							(= (select mid_three R8) (select mid_four R8))
							(= (select mid_three R9) (select mid_four R9))
							(= (select mid_three R10) (select mid_four R10))
							(= (select mid_three R11) (select mid_four R11))
							(= (select mid_three R12) (select mid_four R12))
							(= (select mid_three SP) (select mid_four SP))
							(= (select mid_three LR) (select mid_four LR))

							(or (not (= oper4 CMP)) 
								(ite (= rn4_val flex_val)
									(= ((_ extract 30 30) (select mid_four CPSR)) #b1) ; Z flag3 is 1
									(= ((_ extract 30 30) (select mid_four CPSR)) #b0) ; Z flag3 is 0
								)
							)			
						)
						
						(and
							; R0 to LR equal except destination register
							(or (= (select mid_three R0) (select mid_four R0)) (= rd4 R0))  
							(or (= (select mid_three R1) (select mid_four R1)) (= rd4 R1))
							(or (= (select mid_three R2) (select mid_four R2)) (= rd4 R2))
							(or (= (select mid_three R3) (select mid_four R3)) (= rd4 R3))
							(or (= (select mid_three R4) (select mid_four R4)) (= rd4 R4))
							(or (= (select mid_three R5) (select mid_four R5)) (= rd4 R5))
							(or (= (select mid_three R6) (select mid_four R6)) (= rd4 R6))
							(or (= (select mid_three R7) (select mid_four R7)) (= rd4 R7))
							(or (= (select mid_three R8) (select mid_four R8)) (= rd4 R8))
							(or (= (select mid_three R9) (select mid_four R9)) (= rd4 R9))
							(or (= (select mid_three R10) (select mid_four R10)) (= rd4 R10))
							(or (= (select mid_three R11) (select mid_four R11)) (= rd4 R11))
							(or (= (select mid_three R12) (select mid_four R12)) (= rd4 R12))
							(or (= (select mid_three SP) (select mid_four SP)) (= rd4 SP))
							(or (= (select mid_three LR) (select mid_four LR)) (= rd4 LR))

							(or
								; execute_2
								(and
									(or (= oper4 MOV) (= oper4 MVN))

									(or (not (= oper4 MOV)) (= rd4_val flex_val))
									(or (not (= oper4 MVN)) (= rd4_val (bvnot flex_val)))
								)
								; execute_3
								(and
									(or (= oper4 ADD) (= oper4 SUB) (= oper4 AND)) 

									(or (not (= oper4 ADD)) (= rd4_val (bvadd rn4_val flex_val)))
									(or (not (= oper4 SUB)) (= rd4_val (bvsub rn4_val flex_val)))
									(or (not (= oper4 AND)) (= rd4_val (bvand rn4_val flex_val)))
								)
							)
							; set flag3s or not
							(or 
								(and (= flag4 N) (= (select mid_three CPSR) (select mid_four CPSR))) 
								(and (= flag4 S) 
									(ite 
										(= (select mid_four rd4 ) #b00000000000000000000000000000000) 
										(= ((_ extract 30 30) (select mid_four CPSR)) #b1) ;Z flag3 is 1
										(= ((_ extract 30 30) (select mid_four CPSR)) #b0) ;Z flag3 is 0
									)
								)
							)
						)
					)
				)
			)
		)
	)
))

(assert (=> (transition mid_four mid_five)
	(and
		; pc incremented
		( = (select mid_five PC) (bvadd (select mid_four PC) #b00000000000000000000000000000100))
		(or 
			; instruction not executed
			(and 
				; cond3ition is false
				(not
					(or
						(and (= cond5 EQ) (= ((_ extract 30 30) (select mid_four CPSR)) #b1))
						(and (= cond5 NE) (= ((_ extract 30 30) (select mid_four CPSR)) #b0))
						(and (= cond5 CS) (= ((_ extract 29 29) (select mid_four CPSR)) #b1))
						(and (= cond5 CC) (= ((_ extract 29 29) (select mid_four CPSR)) #b0))
						(and (= cond5 MI) (= ((_ extract 31 31) (select mid_four CPSR)) #b1))
						(and (= cond5 PL) (= ((_ extract 31 31) (select mid_four CPSR)) #b0))
						(and (= cond5 VS) (= ((_ extract 28 28) (select mid_four CPSR)) #b1))
						(and (= cond5 VC) (= ((_ extract 28 28) (select mid_four CPSR)) #b0))

						(and (= cond5 HI) (and (= ((_ extract 29 29) (select mid_four CPSR)) #b1) (= ((_ extract 30 30) (select mid_four CPSR)) #b0) ))
						(and (= cond5 LS) (or (= ((_ extract 29 29) (select mid_four CPSR)) #b0) (= ((_ extract 30 30) (select mid_four CPSR)) #b1) ))
						(and (= cond5 GE) (and (= ((_ extract 31 31) (select mid_four CPSR)) #b1) (= ((_ extract 28 28) (select mid_four CPSR)) #b1) ) )
						(and (= cond5 LT) (or (= ((_ extract 31 31) (select mid_four CPSR)) #b0) (= ((_ extract 28 28) (select mid_four CPSR)) #b0) ) )
						(and 
							(= cond5 GT) 
							(and 
								(= ((_ extract 30 30) (select mid_four CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid_four CPSR)) #b1) (= ((_ extract 28 28) (select mid_four CPSR)) #b1)) 
							)

						)
						(and 
							(= cond5 LE) 
							(or 
								(= ((_ extract 30 30) (select mid_four CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid_four CPSR)) #b1) (= ((_ extract 28 28) (select mid_four CPSR)) #b1))) )
						)
						(= cond5 AL)
					)
				) 
				; R0 to CPSR are equal
				(= (select mid_four R0) (select mid_five R0)) 
				(= (select mid_four R1) (select mid_five R1))
				(= (select mid_four R2) (select mid_five R2))
				(= (select mid_four R3) (select mid_five R3))
				(= (select mid_four R4) (select mid_five R4))
				(= (select mid_four R5) (select mid_five R5))
				(= (select mid_four R6) (select mid_five R6))
				(= (select mid_four R7) (select mid_five R7))
				(= (select mid_four R8) (select mid_five R8))
				(= (select mid_four R9) (select mid_five R9))
				(= (select mid_four R10) (select mid_five R10))
				(= (select mid_four R11) (select mid_five R11))
				(= (select mid_four R12) (select mid_five R12))
				(= (select mid_four SP) (select mid_five SP))
				(= (select mid_four LR) (select mid_five LR)) 
				(= (select mid_four CPSR) (select mid_five CPSR)) 
			)

			(let 
				((flex_val 
					(let ((val_to_shift (ite imm_used5 ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm5))) (select mid_four ro5))))
						(ite
							(= barrel_op5 LSL)
							(bvshl val_to_shift barrel_num5)
							(ite 
								(= barrel_op5 LSR)
								(bvlshr val_to_shift barrel_num5)
								(bvashr val_to_shift barrel_num5)
							)

						)
					)
				)
				(rd5_val (select mid_five rd5 )) (rn5_val (select mid_four rn5 ))
				)

				; instruction is executed
				(and
					; cond3ition is true
					(or
						(and (= cond5 EQ) (= ((_ extract 30 30) (select mid_four CPSR)) #b1))
						(and (= cond5 NE) (= ((_ extract 30 30) (select mid_four CPSR)) #b0))
						(and (= cond5 CS) (= ((_ extract 29 29) (select mid_four CPSR)) #b1))
						(and (= cond5 CC) (= ((_ extract 29 29) (select mid_four CPSR)) #b0))
						(and (= cond5 MI) (= ((_ extract 31 31) (select mid_four CPSR)) #b1))
						(and (= cond5 PL) (= ((_ extract 31 31) (select mid_four CPSR)) #b0))
						(and (= cond5 VS) (= ((_ extract 28 28) (select mid_four CPSR)) #b1))
						(and (= cond5 VC) (= ((_ extract 28 28) (select mid_four CPSR)) #b0))

						(and (= cond5 HI) (and (= ((_ extract 29 29) (select mid_four CPSR)) #b1) (= ((_ extract 30 30) (select mid_four CPSR)) #b0) ))
						(and (= cond5 LS) (or (= ((_ extract 29 29) (select mid_four CPSR)) #b0) (= ((_ extract 30 30) (select mid_four CPSR)) #b1) ))
						(and (= cond5 GE) (and (= ((_ extract 31 31) (select mid_four CPSR)) #b1) (= ((_ extract 28 28) (select mid_four CPSR)) #b1) ) )
						(and (= cond5 LT) (or (= ((_ extract 31 31) (select mid_four CPSR)) #b0) (= ((_ extract 28 28) (select mid_four CPSR)) #b0) ) )
						(and 
							(= cond5 GT) 
							(and 
								(= ((_ extract 30 30) (select mid_four CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid_four CPSR)) #b1) (= ((_ extract 28 28) (select mid_four CPSR)) #b1)) 
							)

						)
						(and 
							(= cond5 LE) 
							(or 
								(= ((_ extract 30 30) (select mid_four CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid_four CPSR)) #b1) (= ((_ extract 28 28) (select mid_four CPSR)) #b1))) )
						)
						(= cond5 AL)
					)
					(or
						; execute_compare
						(and
							(or (= oper5 CMP)) 

							; R0 to LR are equal
							(= (select mid_four R0) (select mid_five R0)) 
							(= (select mid_four R1) (select mid_five R1))
							(= (select mid_four R2) (select mid_five R2))
							(= (select mid_four R3) (select mid_five R3))
							(= (select mid_four R4) (select mid_five R4))
							(= (select mid_four R5) (select mid_five R5))
							(= (select mid_four R6) (select mid_five R6))
							(= (select mid_four R7) (select mid_five R7))
							(= (select mid_four R8) (select mid_five R8))
							(= (select mid_four R9) (select mid_five R9))
							(= (select mid_four R10) (select mid_five R10))
							(= (select mid_four R11) (select mid_five R11))
							(= (select mid_four R12) (select mid_five R12))
							(= (select mid_four SP) (select mid_five SP))
							(= (select mid_four LR) (select mid_five LR))

							(or (not (= oper5 CMP)) 
								(ite (= rn5_val flex_val)
									(= ((_ extract 30 30) (select mid_five CPSR)) #b1) ; Z flag3 is 1
									(= ((_ extract 30 30) (select mid_five CPSR)) #b0) ; Z flag3 is 0
								)
							)			
						)
						
						(and
							; R0 to LR equal except destination register
							(or (= (select mid_four R0) (select mid_five R0)) (= rd5 R0))  
							(or (= (select mid_four R1) (select mid_five R1)) (= rd5 R1))
							(or (= (select mid_four R2) (select mid_five R2)) (= rd5 R2))
							(or (= (select mid_four R3) (select mid_five R3)) (= rd5 R3))
							(or (= (select mid_four R4) (select mid_five R4)) (= rd5 R4))
							(or (= (select mid_four R5) (select mid_five R5)) (= rd5 R5))
							(or (= (select mid_four R6) (select mid_five R6)) (= rd5 R6))
							(or (= (select mid_four R7) (select mid_five R7)) (= rd5 R7))
							(or (= (select mid_four R8) (select mid_five R8)) (= rd5 R8))
							(or (= (select mid_four R9) (select mid_five R9)) (= rd5 R9))
							(or (= (select mid_four R10) (select mid_five R10)) (= rd5 R10))
							(or (= (select mid_four R11) (select mid_five R11)) (= rd5 R11))
							(or (= (select mid_four R12) (select mid_five R12)) (= rd5 R12))
							(or (= (select mid_four SP) (select mid_five SP)) (= rd5 SP))
							(or (= (select mid_four LR) (select mid_five LR)) (= rd5 LR))

							(or
								; execute_2
								(and
									(or (= oper5 MOV) (= oper5 MVN))

									(or (not (= oper5 MOV)) (= rd5_val flex_val))
									(or (not (= oper5 MVN)) (= rd5_val (bvnot flex_val)))
								)
								; execute_3
								(and
									(or (= oper5 ADD) (= oper5 SUB) (= oper5 AND)) 

									(or (not (= oper5 ADD)) (= rd5_val (bvadd rn5_val flex_val)))
									(or (not (= oper5 SUB)) (= rd5_val (bvsub rn5_val flex_val)))
									(or (not (= oper5 AND)) (= rd5_val (bvand rn5_val flex_val)))
								)
							)
							; set flag3s or not
							(or 
								(and (= flag5 N) (= (select mid_four CPSR) (select mid_five CPSR))) 
								(and (= flag5 S) 
									(ite 
										(= (select mid_five rd5 ) #b00000000000000000000000000000000) 
										(= ((_ extract 30 30) (select mid_five CPSR)) #b1) ;Z flag3 is 1
										(= ((_ extract 30 30) (select mid_five CPSR)) #b0) ;Z flag3 is 0
									)
								)
							)
						)
					)
				)
			)
		)
	)
))

(assert (=> (transition mid_five post)
	(and
		; pc incremented
		( = (select post PC) (bvadd (select mid_five PC) #b00000000000000000000000000000100))
		(or 
			; instruction not executed
			(and 
				; cond3ition is false
				(not
					(or
						(and (= cond6 EQ) (= ((_ extract 30 30) (select mid_five CPSR)) #b1))
						(and (= cond6 NE) (= ((_ extract 30 30) (select mid_five CPSR)) #b0))
						(and (= cond6 CS) (= ((_ extract 29 29) (select mid_five CPSR)) #b1))
						(and (= cond6 CC) (= ((_ extract 29 29) (select mid_five CPSR)) #b0))
						(and (= cond6 MI) (= ((_ extract 31 31) (select mid_five CPSR)) #b1))
						(and (= cond6 PL) (= ((_ extract 31 31) (select mid_five CPSR)) #b0))
						(and (= cond6 VS) (= ((_ extract 28 28) (select mid_five CPSR)) #b1))
						(and (= cond6 VC) (= ((_ extract 28 28) (select mid_five CPSR)) #b0))

						(and (= cond6 HI) (and (= ((_ extract 29 29) (select mid_five CPSR)) #b1) (= ((_ extract 30 30) (select mid_five CPSR)) #b0) ))
						(and (= cond6 LS) (or (= ((_ extract 29 29) (select mid_five CPSR)) #b0) (= ((_ extract 30 30) (select mid_five CPSR)) #b1) ))
						(and (= cond6 GE) (and (= ((_ extract 31 31) (select mid_five CPSR)) #b1) (= ((_ extract 28 28) (select mid_five CPSR)) #b1) ) )
						(and (= cond6 LT) (or (= ((_ extract 31 31) (select mid_five CPSR)) #b0) (= ((_ extract 28 28) (select mid_five CPSR)) #b0) ) )
						(and 
							(= cond6 GT) 
							(and 
								(= ((_ extract 30 30) (select mid_five CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid_five CPSR)) #b1) (= ((_ extract 28 28) (select mid_five CPSR)) #b1)) 
							)

						)
						(and 
							(= cond6 LE) 
							(or 
								(= ((_ extract 30 30) (select mid_five CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid_five CPSR)) #b1) (= ((_ extract 28 28) (select mid_five CPSR)) #b1))) )
						)
						(= cond6 AL)
					)
				) 
				; R0 to CPSR are equal
				(= (select mid_five R0) (select post R0)) 
				(= (select mid_five R1) (select post R1))
				(= (select mid_five R2) (select post R2))
				(= (select mid_five R3) (select post R3))
				(= (select mid_five R4) (select post R4))
				(= (select mid_five R5) (select post R5))
				(= (select mid_five R6) (select post R6))
				(= (select mid_five R7) (select post R7))
				(= (select mid_five R8) (select post R8))
				(= (select mid_five R9) (select post R9))
				(= (select mid_five R10) (select post R10))
				(= (select mid_five R11) (select post R11))
				(= (select mid_five R12) (select post R12))
				(= (select mid_five SP) (select post SP))
				(= (select mid_five LR) (select post LR)) 
				(= (select mid_five CPSR) (select post CPSR)) 
			)

			(let 
				((flex_val 
					(let ((val_to_shift (ite imm_used6 ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm6))) (select mid_five ro6))))
						(ite
							(= barrel_op6 LSL)
							(bvshl val_to_shift barrel_num6)
							(ite 
								(= barrel_op6 LSR)
								(bvlshr val_to_shift barrel_num6)
								(bvashr val_to_shift barrel_num6)
							)

						)
					)
				)
				(rd6_val (select post rd6 )) (rn6_val (select mid_five rn6 ))
				)

				; instruction is executed
				(and
					; cond3ition is true
					(or
						(and (= cond6 EQ) (= ((_ extract 30 30) (select mid_five CPSR)) #b1))
						(and (= cond6 NE) (= ((_ extract 30 30) (select mid_five CPSR)) #b0))
						(and (= cond6 CS) (= ((_ extract 29 29) (select mid_five CPSR)) #b1))
						(and (= cond6 CC) (= ((_ extract 29 29) (select mid_five CPSR)) #b0))
						(and (= cond6 MI) (= ((_ extract 31 31) (select mid_five CPSR)) #b1))
						(and (= cond6 PL) (= ((_ extract 31 31) (select mid_five CPSR)) #b0))
						(and (= cond6 VS) (= ((_ extract 28 28) (select mid_five CPSR)) #b1))
						(and (= cond6 VC) (= ((_ extract 28 28) (select mid_five CPSR)) #b0))

						(and (= cond6 HI) (and (= ((_ extract 29 29) (select mid_five CPSR)) #b1) (= ((_ extract 30 30) (select mid_five CPSR)) #b0) ))
						(and (= cond6 LS) (or (= ((_ extract 29 29) (select mid_five CPSR)) #b0) (= ((_ extract 30 30) (select mid_five CPSR)) #b1) ))
						(and (= cond6 GE) (and (= ((_ extract 31 31) (select mid_five CPSR)) #b1) (= ((_ extract 28 28) (select mid_five CPSR)) #b1) ) )
						(and (= cond6 LT) (or (= ((_ extract 31 31) (select mid_five CPSR)) #b0) (= ((_ extract 28 28) (select mid_five CPSR)) #b0) ) )
						(and 
							(= cond6 GT) 
							(and 
								(= ((_ extract 30 30) (select mid_five CPSR)) #b0) 
								(= (= ((_ extract 31 31) (select mid_five CPSR)) #b1) (= ((_ extract 28 28) (select mid_five CPSR)) #b1)) 
							)

						)
						(and 
							(= cond6 LE) 
							(or 
								(= ((_ extract 30 30) (select mid_five CPSR)) #b1) 
								(not(= (= ((_ extract 31 31) (select mid_five CPSR)) #b1) (= ((_ extract 28 28) (select mid_five CPSR)) #b1))) )
						)
						(= cond6 AL)
					)
					(or
						; execute_compare
						(and
							(or (= oper6 CMP)) 

							; R0 to LR are equal
							(= (select mid_five R0) (select post R0)) 
							(= (select mid_five R1) (select post R1))
							(= (select mid_five R2) (select post R2))
							(= (select mid_five R3) (select post R3))
							(= (select mid_five R4) (select post R4))
							(= (select mid_five R5) (select post R5))
							(= (select mid_five R6) (select post R6))
							(= (select mid_five R7) (select post R7))
							(= (select mid_five R8) (select post R8))
							(= (select mid_five R9) (select post R9))
							(= (select mid_five R10) (select post R10))
							(= (select mid_five R11) (select post R11))
							(= (select mid_five R12) (select post R12))
							(= (select mid_five SP) (select post SP))
							(= (select mid_five LR) (select post LR))

							(or (not (= oper6 CMP)) 
								(ite (= rn6_val flex_val)
									(= ((_ extract 30 30) (select post CPSR)) #b1) ; Z flag3 is 1
									(= ((_ extract 30 30) (select post CPSR)) #b0) ; Z flag3 is 0
								)
							)			
						)
						
						(and
							; R0 to LR equal except destination register
							(or (= (select mid_five R0) (select post R0)) (= rd6 R0))  
							(or (= (select mid_five R1) (select post R1)) (= rd6 R1))
							(or (= (select mid_five R2) (select post R2)) (= rd6 R2))
							(or (= (select mid_five R3) (select post R3)) (= rd6 R3))
							(or (= (select mid_five R4) (select post R4)) (= rd6 R4))
							(or (= (select mid_five R5) (select post R5)) (= rd6 R5))
							(or (= (select mid_five R6) (select post R6)) (= rd6 R6))
							(or (= (select mid_five R7) (select post R7)) (= rd6 R7))
							(or (= (select mid_five R8) (select post R8)) (= rd6 R8))
							(or (= (select mid_five R9) (select post R9)) (= rd6 R9))
							(or (= (select mid_five R10) (select post R10)) (= rd6 R10))
							(or (= (select mid_five R11) (select post R11)) (= rd6 R11))
							(or (= (select mid_five R12) (select post R12)) (= rd6 R12))
							(or (= (select mid_five SP) (select post SP)) (= rd6 SP))
							(or (= (select mid_five LR) (select post LR)) (= rd6 LR))

							(or
								; execute_2
								(and
									(or (= oper6 MOV) (= oper6 MVN))

									(or (not (= oper6 MOV)) (= rd6_val flex_val))
									(or (not (= oper6 MVN)) (= rd6_val (bvnot flex_val)))
								)
								; execute_3
								(and
									(or (= oper6 ADD) (= oper6 SUB) (= oper6 AND)) 

									(or (not (= oper6 ADD)) (= rd6_val (bvadd rn6_val flex_val)))
									(or (not (= oper6 SUB)) (= rd6_val (bvsub rn6_val flex_val)))
									(or (not (= oper6 AND)) (= rd6_val (bvand rn6_val flex_val)))
								)
							)
							; set flag3s or not
							(or 
								(and (= flag6 N) (= (select mid_five CPSR) (select post CPSR))) 
								(and (= flag6 S) 
									(ite 
										(= (select post rd6 ) #b00000000000000000000000000000000) 
										(= ((_ extract 30 30) (select post CPSR)) #b1) ;Z flag3 is 1
										(= ((_ extract 30 30) (select post CPSR)) #b0) ;Z flag3 is 0
									)
								)
							)
						)
					)
				)
			)
		)
	)
))

(assert (transition pre mid))
(assert (transition mid mid_two))
(assert (transition mid_two mid_three))
(assert (transition mid_three mid_four))
(assert (transition mid_four mid_five))
(assert (transition mid_five post))

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

(assert (= (select post R0) #b00000000000000000000000000001111))
(assert (= (select post R1) #b00000000000000000000000000001111))
(assert (= (select post R2) #b00000000000000000000000000010000))
(assert (= (select post R3) #b00000000000000000000000000100000))
(assert (= (select post R4) #b00000000000000000000000001000000))
(assert (= (select post R5) #b00000000000000000000000010000000))
(assert (= (select post R6) #b00000000000000000000000100000000))
(assert (= (select post R7) #b00000000000000000000000000000000))
(assert (= (select post R8) #b00000000000000000000000000000000))
(assert (= (select post R9) #b00000000000000000000000000000000))
(assert (= (select post R10) #b00000000000000000000000000000000))
(assert (= (select post R11) #b00000000000000000000000000000000))
(assert (= (select post R12) #b00000000000000000000000000000000))
(assert (= (select post SP) #b00000000000000000000000000000000))
(assert (= (select post LR) #b00000000000000000000000000000000))
(assert (= (select post PC) #b00000000000000000000000000011000))
(assert (= (select post CPSR) #b01000000000000000000000000000000))

;(assert (= flag S))
;(assert (= cond EQ))

(check-sat)

;(get-model)

(echo "")

(eval oper) 
(eval cond)
(eval flag)

(eval rd)
(eval rn)
(eval ro)

(eval imm_used)
(eval imm)

(eval barrel_op)
(eval barrel_num)

(echo "")

(eval oper2) 
(eval cond2)
(eval flag2)

(eval rd2)
(eval rn2)
(eval ro2)

(eval imm_used2)
(eval imm2)

(eval barrel_op2)
(eval barrel_num2)

(echo "")

(eval oper3) 
(eval cond3)
(eval flag3)

(eval rd3)
(eval rn3)
(eval ro3)

(eval imm_used3)
(eval imm3)

(eval barrel_op3)
(eval barrel_num3)

(echo "")

(eval oper4) 
(eval cond4)
(eval flag4)

(eval rd4)
(eval rn4)
(eval ro4)

(eval imm_used4)
(eval imm4)

(eval barrel_op4)
(eval barrel_num4)

(echo "")

(eval oper5) 
(eval cond5)
(eval flag5)

(eval rd5)
(eval rn5)
(eval ro5)

(eval imm_used5)
(eval imm5)

(eval barrel_op5)
(eval barrel_num5)

(echo "")

(eval oper6) 
(eval cond6)
(eval flag6)

(eval rd6)
(eval rn6)
(eval ro6)

(eval imm_used6)
(eval imm6)

(eval barrel_op6)
(eval barrel_num6)

; TODO
; more flexible templates for operations V
; immediate value shifting/rotating V need to implement rotations
; instructions with barrel shifter V need to implement rotations
; negative, carry, overflow flags setting
; more readable output, says what format the instruction is: 2/3/imm args etc V

; nest function/logical formalulae more efficiently (reduce redundancy) V
; encapsulate oper cond flag etc in bitvec 32
; enable solving of multiple intermediate states V can only solve hardcoded sequence of states




















