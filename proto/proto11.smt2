(echo "Generating ARM contraints")

(declare-datatypes () ((Reg R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 SP LR PC CPSR)))
(declare-datatypes () ((Cond EQ NE CS CC MI PL VS VC HI LS GE LT GT LE AL)))
(declare-datatypes () ((Op MOV MVN CMP ADD SUB AND ORR EOR BIC)))
(declare-datatypes () ((Flag N S)))
(define-sort State () (Array Reg (_ BitVec 32)))

(declare-var pre State)
(declare-var post State)

(declare-var o Op)
(declare-var c Cond)
(declare-var x Reg)
(declare-var y Reg)
(declare-var z Reg)

(declare-rel transition3 (State Op Cond Reg Reg Reg State))
(declare-rel eval_true (State Cond))
(declare-rel R0_to_LR_equal_except (State State Reg))
(declare-rel R0_to_LR_equal (State State))
(declare-rel CPSR_equal (State State))
(declare-rel pc_incremented (State State))

(declare-rel N1 (State))
(declare-rel Z1 (State))
(declare-rel C1 (State))
(declare-rel V1 (State))

(assert (=> (R0_to_LR_equal_except pre post x) 
	(and 
		(or (= (select pre R0) (select post R0)) (= x R0))  
		(or (= (select pre R1) (select post R1)) (= x R1))
		(or (= (select pre R2) (select post R2)) (= x R2))
		(or (= (select pre R3) (select post R3)) (= x R3))
		(or (= (select pre R4) (select post R4)) (= x R4))
		(or (= (select pre R5) (select post R5)) (= x R5))
		(or (= (select pre R6) (select post R6)) (= x R6))
		(or (= (select pre R7) (select post R7)) (= x R7))
		(or (= (select pre R8) (select post R8)) (= x R8))
		(or (= (select pre R9) (select post R9)) (= x R9))
		(or (= (select pre R10) (select post R10)) (= x R10))
		(or (= (select pre R11) (select post R11)) (= x R11))
		(or (= (select pre R12) (select post R12)) (= x R12))
		(or (= (select pre SP) (select post SP)) (= x SP))
		(or (= (select pre LR) (select post LR)) (= x LR))
	)))

(assert (=> (R0_to_LR_equal pre post) (and 
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
	)))

(assert (=> (CPSR_equal pre post)
	(= (select pre CPSR) (select post CPSR))
	))

(assert (=> (pc_incremented pre post)
	(= (select post PC) (bvadd (select pre PC) #b00000000000000000000000000000100))
	))


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

(assert (=> (transition3 pre ADD c x y z post) 
	(or 
		(and (eval_true pre c) (R0_to_LR_equal_except pre post x) (CPSR_equal pre post) (= (select post x) (bvadd (select pre y) (select pre z))) (pc_incremented pre post)) 
		(and (not (eval_true pre c)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post)) 
		)))

(assert (=> (transition3 pre SUB c x y z post) 
	(or 
		(and (eval_true pre c) (R0_to_LR_equal_except pre post x) (CPSR_equal pre post) (= (select post x) (bvsub (select pre y) (select pre z))) (pc_incremented pre post)) 
		(and (not (eval_true pre c)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post)) 
		)))

(assert (=> (transition3 pre MOV c x y z post)
	(or 
		(and (eval_true pre c) (R0_to_LR_equal_except pre post x) (CPSR_equal pre post) (= (select pre y) (select post x)) (pc_incremented pre post))
		(and (not (eval_true pre c)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
		)))

(assert (=> (transition3 pre MVN c x y z post)
	(or 
		(and (eval_true pre c) (R0_to_LR_equal_except pre post x) (CPSR_equal pre post) (= (bvnot (select pre y)) (select post x)) (pc_incremented pre post))
		(and (not (eval_true pre c)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
		)))

(assert (=> (transition3 pre CMP c x y z post) 
	(or 
		(and (eval_true pre c) (R0_to_LR_equal pre post) (ite (= (select pre x) (select pre y)) (Z1 post) (not (Z1 post))) (pc_incremented pre post)) 
		(and (not (eval_true pre c)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
		)))

(assert (=> (transition3 pre AND c x y z post)
	(or
		(and (eval_true pre c) (R0_to_LR_equal_except pre post x) (CPSR_equal pre post) (= (select post x) (bvand (select pre y) (select pre z))) (pc_incremented pre post))
		(and (not (eval_true pre c)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
		)
	))

(assert (=> (transition3 pre ORR c x y z post)
	(or
		(and (eval_true pre c) (R0_to_LR_equal_except pre post x) (CPSR_equal pre post) (= (select post x) (bvor (select pre y) (select pre z))) (pc_incremented pre post))
		(and (not (eval_true pre c)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
		)
	))

(assert (=> (transition3 pre EOR c x y z post)
	(or
		(and (eval_true pre c) (R0_to_LR_equal_except pre post x) (CPSR_equal pre post) (= (select post x) (bvxor (select pre y) (select pre z))) (pc_incremented pre post))
		(and (not (eval_true pre c)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
		)
	))

(assert (=> (transition3 pre BIC c x y z post)
	(or
		(and (eval_true pre c) (R0_to_LR_equal_except pre post x) (CPSR_equal pre post) (= (select post x) (bvand (select pre y) (bvnot (select pre z)) )) (pc_incremented pre post))
		(and (not (eval_true pre c)) (R0_to_LR_equal pre post) (CPSR_equal pre post) (pc_incremented pre post))
		)
	))

(assert (transition3 pre o c x y z post))

(assert (= (select pre R0) #b00000000000000000000000000000000))
(assert (= (select pre R1) #b00000000000000000000000000011110))
(assert (= (select pre R2) #b00000000000000000000000000000011))
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
(assert (= (select pre CPSR) #b00000000000000000000000000000000))

(assert (= (select post R0) #b00000000000000000000000000000000))
(assert (= (select post R1) #b00000000000000000000000000011110))
(assert (= (select post R2) #b00000000000000000000000000000011))
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

(check-sat)

(get-model)





































