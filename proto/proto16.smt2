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

(assert (=> (transition pre post)
	(and
		; pc incremented
		(= (select post PC) (bvadd (select pre PC) #b00000000000000000000000000000100))
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
				(= (select pre CPSR) (select post CPSR)) 
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
				(rd_val (select post rd)) (rn_val (select pre rn))
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

							(or (not (= oper CMP)) 
								(ite (= rn_val flex_val)
									(= ((_ extract 30 30) (select post CPSR)) #b1) ; Z flag is 1
									(= ((_ extract 30 30) (select post CPSR)) #b0) ; Z flag is 0
								)
							)			
						)
						
						(and
							; R0 to LR equal except destination register
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
								(and (= flag N) (= (select pre CPSR) (select post CPSR))) 
								(and (= flag S) 
									(ite 
										(= (select post rd) #b00000000000000000000000000000000) 
										(= ((_ extract 30 30) (select post CPSR)) #b1) ;Z flag is 1
										(= ((_ extract 30 30) (select post CPSR)) #b0) ;Z flag is 0
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

(assert (= (select post R0) #b00000000000000000000000000001111))
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

; TODO
; more flexible templates for operations V
; immediate value shifting/rotating V need to implement rotations
; instructions with barrel shifter V need to implement rotations
; negative, carry, overflow flags setting
; more readable output, says what format the instruction is: 2/3/imm args etc V

; nest function/logical formalulae more efficiently (reduce redundancy) V
; encapsulate oper cond flag etc in bitvec 32
; enable solving of multiple intermediate states




















