(echo "Generating ARM V8 Constraints")
(declare-datatypes () ((Operation ADD SUB MOV MVN CMP AND)))
(declare-datatypes () ((Condition EQ NE CS CC MI PL VS VC HI LS GE LT GT LE AL)))
(declare-datatypes () ((Flag N S)))
(declare-datatypes () ((BarrelOp LSL LSR ASR)))
(declare-datatypes () ((Register R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 SP LR PC CPSR)))
(define-sort State () (Array Register (_ BitVec 32)))
(declare-rel transition (State State))
(declare-var state0 State)
(declare-var oper0 Operation)
(declare-var cond0 Condition)
(declare-var flag0 Flag)
(declare-var rd0 Register)
(declare-var rn0 Register)
(declare-var ro0 Register)
(declare-var imm0 (_ BitVec 12))
(declare-var imm0_used Bool)
(declare-var barrel_op0 BarrelOp)
(declare-var barrel_num0 (_ BitVec 32))
(declare-var state1 State)
(declare-var oper1 Operation)
(declare-var cond1 Condition)
(declare-var flag1 Flag)
(declare-var rd1 Register)
(declare-var rn1 Register)
(declare-var ro1 Register)
(declare-var imm1 (_ BitVec 12))
(declare-var imm1_used Bool)
(declare-var barrel_op1 BarrelOp)
(declare-var barrel_num1 (_ BitVec 32))
(declare-var state2 State)
(declare-var oper2 Operation)
(declare-var cond2 Condition)
(declare-var flag2 Flag)
(declare-var rd2 Register)
(declare-var rn2 Register)
(declare-var ro2 Register)
(declare-var imm2 (_ BitVec 12))
(declare-var imm2_used Bool)
(declare-var barrel_op2 BarrelOp)
(declare-var barrel_num2 (_ BitVec 32))
(declare-var state3 State)
(declare-var oper3 Operation)
(declare-var cond3 Condition)
(declare-var flag3 Flag)
(declare-var rd3 Register)
(declare-var rn3 Register)
(declare-var ro3 Register)
(declare-var imm3 (_ BitVec 12))
(declare-var imm3_used Bool)
(declare-var barrel_op3 BarrelOp)
(declare-var barrel_num3 (_ BitVec 32))
(declare-var state4 State)
(declare-var oper4 Operation)
(declare-var cond4 Condition)
(declare-var flag4 Flag)
(declare-var rd4 Register)
(declare-var rn4 Register)
(declare-var ro4 Register)
(declare-var imm4 (_ BitVec 12))
(declare-var imm4_used Bool)
(declare-var barrel_op4 BarrelOp)
(declare-var barrel_num4 (_ BitVec 32))
(declare-var state5 State)
(declare-var oper5 Operation)
(declare-var cond5 Condition)
(declare-var flag5 Flag)
(declare-var rd5 Register)
(declare-var rn5 Register)
(declare-var ro5 Register)
(declare-var imm5 (_ BitVec 12))
(declare-var imm5_used Bool)
(declare-var barrel_op5 BarrelOp)
(declare-var barrel_num5 (_ BitVec 32))
(declare-var state6 State)
(declare-var oper6 Operation)
(declare-var cond6 Condition)
(declare-var flag6 Flag)
(declare-var rd6 Register)
(declare-var rn6 Register)
(declare-var ro6 Register)
(declare-var imm6 (_ BitVec 12))
(declare-var imm6_used Bool)
(declare-var barrel_op6 BarrelOp)
(declare-var barrel_num6 (_ BitVec 32))
(declare-var state7 State)
(declare-var oper7 Operation)
(declare-var cond7 Condition)
(declare-var flag7 Flag)
(declare-var rd7 Register)
(declare-var rn7 Register)
(declare-var ro7 Register)
(declare-var imm7 (_ BitVec 12))
(declare-var imm7_used Bool)
(declare-var barrel_op7 BarrelOp)
(declare-var barrel_num7 (_ BitVec 32))
(declare-var state8 State)
(declare-var oper8 Operation)
(declare-var cond8 Condition)
(declare-var flag8 Flag)
(declare-var rd8 Register)
(declare-var rn8 Register)
(declare-var ro8 Register)
(declare-var imm8 (_ BitVec 12))
(declare-var imm8_used Bool)
(declare-var barrel_op8 BarrelOp)
(declare-var barrel_num8 (_ BitVec 32))
(declare-var state9 State)
(declare-var oper9 Operation)
(declare-var cond9 Condition)
(declare-var flag9 Flag)
(declare-var rd9 Register)
(declare-var rn9 Register)
(declare-var ro9 Register)
(declare-var imm9 (_ BitVec 12))
(declare-var imm9_used Bool)
(declare-var barrel_op9 BarrelOp)
(declare-var barrel_num9 (_ BitVec 32))
(declare-var state10 State)
(declare-var oper10 Operation)
(declare-var cond10 Condition)
(declare-var flag10 Flag)
(declare-var rd10 Register)
(declare-var rn10 Register)
(declare-var ro10 Register)
(declare-var imm10 (_ BitVec 12))
(declare-var imm10_used Bool)
(declare-var barrel_op10 BarrelOp)
(declare-var barrel_num10 (_ BitVec 32))
(declare-var state11 State)
(declare-var oper11 Operation)
(declare-var cond11 Condition)
(declare-var flag11 Flag)
(declare-var rd11 Register)
(declare-var rn11 Register)
(declare-var ro11 Register)
(declare-var imm11 (_ BitVec 12))
(declare-var imm11_used Bool)
(declare-var barrel_op11 BarrelOp)
(declare-var barrel_num11 (_ BitVec 32))
(declare-var state12 State)
(declare-var oper12 Operation)
(declare-var cond12 Condition)
(declare-var flag12 Flag)
(declare-var rd12 Register)
(declare-var rn12 Register)
(declare-var ro12 Register)
(declare-var imm12 (_ BitVec 12))
(declare-var imm12_used Bool)
(declare-var barrel_op12 BarrelOp)
(declare-var barrel_num12 (_ BitVec 32))
(declare-var state13 State)
(declare-var oper13 Operation)
(declare-var cond13 Condition)
(declare-var flag13 Flag)
(declare-var rd13 Register)
(declare-var rn13 Register)
(declare-var ro13 Register)
(declare-var imm13 (_ BitVec 12))
(declare-var imm13_used Bool)
(declare-var barrel_op13 BarrelOp)
(declare-var barrel_num13 (_ BitVec 32))
(declare-var state14 State)
(declare-var oper14 Operation)
(declare-var cond14 Condition)
(declare-var flag14 Flag)
(declare-var rd14 Register)
(declare-var rn14 Register)
(declare-var ro14 Register)
(declare-var imm14 (_ BitVec 12))
(declare-var imm14_used Bool)
(declare-var barrel_op14 BarrelOp)
(declare-var barrel_num14 (_ BitVec 32))
(declare-var state15 State)
(declare-var oper15 Operation)
(declare-var cond15 Condition)
(declare-var flag15 Flag)
(declare-var rd15 Register)
(declare-var rn15 Register)
(declare-var ro15 Register)
(declare-var imm15 (_ BitVec 12))
(declare-var imm15_used Bool)
(declare-var barrel_op15 BarrelOp)
(declare-var barrel_num15 (_ BitVec 32))
(declare-var state16 State)
(declare-var oper16 Operation)
(declare-var cond16 Condition)
(declare-var flag16 Flag)
(declare-var rd16 Register)
(declare-var rn16 Register)
(declare-var ro16 Register)
(declare-var imm16 (_ BitVec 12))
(declare-var imm16_used Bool)
(declare-var barrel_op16 BarrelOp)
(declare-var barrel_num16 (_ BitVec 32))
(assert (=> (transition state0 state1)
(and
; pc incremented
(= (select state1 PC) (bvadd (select state0 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond0ition is false
(not
(or
(and (= cond0 EQ) (= ((_ extract 30 30) (select state0 CPSR)) #b1))
(and (= cond0 NE) (= ((_ extract 30 30) (select state0 CPSR)) #b0))
(and (= cond0 CS) (= ((_ extract 29 29) (select state0 CPSR)) #b1))
(and (= cond0 CC) (= ((_ extract 29 29) (select state0 CPSR)) #b0))
(and (= cond0 MI) (= ((_ extract 31 31) (select state0 CPSR)) #b1))
(and (= cond0 PL) (= ((_ extract 31 31) (select state0 CPSR)) #b0))
(and (= cond0 VS) (= ((_ extract 28 28) (select state0 CPSR)) #b1))
(and (= cond0 VC) (= ((_ extract 28 28) (select state0 CPSR)) #b0))
(and (= cond0 HI) (and (= ((_ extract 29 29) (select state0 CPSR)) #b1) (= ((_ extract 30 30) (select state0 CPSR)) #b0) ))
(and (= cond0 LS) (or (= ((_ extract 29 29) (select state0 CPSR)) #b0) (= ((_ extract 30 30) (select state0 CPSR)) #b1) ))
(and (= cond0 GE) (and (= ((_ extract 31 31) (select state0 CPSR)) #b1) (= ((_ extract 28 28) (select state0 CPSR)) #b1) ) )
(and (= cond0 LT) (or (= ((_ extract 31 31) (select state0 CPSR)) #b0) (= ((_ extract 28 28) (select state0 CPSR)) #b0) ) )
(and 
(= cond0 GT)
(and 
(= ((_ extract 30 30) (select state0 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state0 CPSR)) #b1) (= ((_ extract 28 28) (select state0 CPSR)) #b1)) 
)
)
(and 
(= cond0 LE)
(or 
(= ((_ extract 30 30) (select state0 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state0 CPSR)) #b1) (= ((_ extract 28 28) (select state0 CPSR)) #b1))) )
)
(= cond0 AL)
)
) 
; R0 to CPSR are equal
(= (select state0 R0) (select state1 R0)) 
(= (select state0 R1) (select state1 R1))
(= (select state0 R2) (select state1 R2))
(= (select state0 R3) (select state1 R3))
(= (select state0 R4) (select state1 R4))
(= (select state0 R5) (select state1 R5))
(= (select state0 R6) (select state1 R6))
(= (select state0 R7) (select state1 R7))
(= (select state0 R8) (select state1 R8))
(= (select state0 R9) (select state1 R9))
(= (select state0 R10) (select state1 R10))
(= (select state0 R11) (select state1 R11))
(= (select state0 R12) (select state1 R12))
(= (select state0 SP) (select state1 SP))
(= (select state0 LR) (select state1 LR)) 
(= (select state0 CPSR) (select state1 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm0_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm0))) (select state0 ro0))))
(ite
(= barrel_op0 LSL)
(bvshl val_to_shift barrel_num0)
(ite 
(= barrel_op0 LSR)
(bvlshr val_to_shift barrel_num0)
(bvashr val_to_shift barrel_num0)
)
)
)
)
(rd0_val (select state1 rd0)) (rn0_val (select state0 rn0))
)
; instruction is executed
(and
; cond0ition is true
(or
(and (= cond0 EQ) (= ((_ extract 30 30) (select state0 CPSR)) #b1))
(and (= cond0 NE) (= ((_ extract 30 30) (select state0 CPSR)) #b0))
(and (= cond0 CS) (= ((_ extract 29 29) (select state0 CPSR)) #b1))
(and (= cond0 CC) (= ((_ extract 29 29) (select state0 CPSR)) #b0))
(and (= cond0 MI) (= ((_ extract 31 31) (select state0 CPSR)) #b1))
(and (= cond0 PL) (= ((_ extract 31 31) (select state0 CPSR)) #b0))
(and (= cond0 VS) (= ((_ extract 28 28) (select state0 CPSR)) #b1))
(and (= cond0 VC) (= ((_ extract 28 28) (select state0 CPSR)) #b0))
(and (= cond0 HI) (and (= ((_ extract 29 29) (select state0 CPSR)) #b1) (= ((_ extract 30 30) (select state0 CPSR)) #b0) ))
(and (= cond0 LS) (or (= ((_ extract 29 29) (select state0 CPSR)) #b0) (= ((_ extract 30 30) (select state0 CPSR)) #b1) ))
(and (= cond0 GE) (and (= ((_ extract 31 31) (select state0 CPSR)) #b1) (= ((_ extract 28 28) (select state0 CPSR)) #b1) ) )
(and (= cond0 LT) (or (= ((_ extract 31 31) (select state0 CPSR)) #b0) (= ((_ extract 28 28) (select state0 CPSR)) #b0) ) )
(and 
(= cond0 GT) 
(and 
(= ((_ extract 30 30) (select state0 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state0 CPSR)) #b1) (= ((_ extract 28 28) (select state0 CPSR)) #b1)) 
)
)
(and 
(= cond0 LE)
(or 
(= ((_ extract 30 30) (select state0 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state0 CPSR)) #b1) (= ((_ extract 28 28) (select state0 CPSR)) #b1))) )
)
(= cond0 AL)
)
(or
; execute_compare
(and
(or (= oper0 CMP)) 
; R0 to LR are equal
(= (select state0 R0) (select state1 R0)) 
(= (select state0 R1) (select state1 R1))
(= (select state0 R2) (select state1 R2))
(= (select state0 R3) (select state1 R3))
(= (select state0 R4) (select state1 R4))
(= (select state0 R5) (select state1 R5))
(= (select state0 R6) (select state1 R6))
(= (select state0 R7) (select state1 R7))
(= (select state0 R8) (select state1 R8))
(= (select state0 R9) (select state1 R9))
(= (select state0 R10) (select state1 R10))
(= (select state0 R11) (select state1 R11))
(= (select state0 R12) (select state1 R12))
(= (select state0 SP) (select state1 SP))
(= (select state0 LR) (select state1 LR))
(or (not (= oper0 CMP)) 
(ite (= rn0_val flex_val)
(= ((_ extract 30 30) (select state1 CPSR)) #b1) ; Z flag0 is 1 
(= ((_ extract 30 30) (select state1 CPSR)) #b0) ; Z flag0 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state0 R0) (select state1 R0)) (= rd0 R0))  
(or (= (select state0 R1) (select state1 R1)) (= rd0 R1))
(or (= (select state0 R2) (select state1 R2)) (= rd0 R2))
(or (= (select state0 R3) (select state1 R3)) (= rd0 R3))
(or (= (select state0 R4) (select state1 R4)) (= rd0 R4))
(or (= (select state0 R5) (select state1 R5)) (= rd0 R5))
(or (= (select state0 R6) (select state1 R6)) (= rd0 R6))
(or (= (select state0 R7) (select state1 R7)) (= rd0 R7))
(or (= (select state0 R8) (select state1 R8)) (= rd0 R8))
(or (= (select state0 R9) (select state1 R9)) (= rd0 R9))
(or (= (select state0 R10) (select state1 R10)) (= rd0 R10))
(or (= (select state0 R11) (select state1 R11)) (= rd0 R11))
(or (= (select state0 R12) (select state1 R12)) (= rd0 R12))
(or (= (select state0 SP) (select state1 SP)) (= rd0 SP))
(or (= (select state0 LR) (select state1 LR)) (= rd0 LR))
(or
; execute_2
(and
(or (= oper0 MOV) (= oper0 MVN))
(or (not (= oper0 MOV)) (= rd0_val flex_val))
(or (not (= oper0 MVN)) (= rd0_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper0 ADD) (= oper0 SUB) (= oper0 AND)) 
(or (not (= oper0 ADD)) (= rd0_val (bvadd rn0_val flex_val)))
(or (not (= oper0 SUB)) (= rd0_val (bvsub rn0_val flex_val)))
(or (not (= oper0 AND)) (= rd0_val (bvand rn0_val flex_val)))
)
)
; set flag0s or not
(or 
(and (= flag0 N) (= (select state0 CPSR) (select state1 CPSR))) 
(and (= flag0 S) 
(ite 
(= (select state1 rd0) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state1 CPSR)) #b1) ;Z flag0 is 1
(= ((_ extract 30 30) (select state1 CPSR)) #b0) ;Z flag0 is 0
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
(assert (=> (transition state1 state2)
(and
; pc incremented
(= (select state2 PC) (bvadd (select state1 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond1ition is false
(not
(or
(and (= cond1 EQ) (= ((_ extract 30 30) (select state1 CPSR)) #b1))
(and (= cond1 NE) (= ((_ extract 30 30) (select state1 CPSR)) #b0))
(and (= cond1 CS) (= ((_ extract 29 29) (select state1 CPSR)) #b1))
(and (= cond1 CC) (= ((_ extract 29 29) (select state1 CPSR)) #b0))
(and (= cond1 MI) (= ((_ extract 31 31) (select state1 CPSR)) #b1))
(and (= cond1 PL) (= ((_ extract 31 31) (select state1 CPSR)) #b0))
(and (= cond1 VS) (= ((_ extract 28 28) (select state1 CPSR)) #b1))
(and (= cond1 VC) (= ((_ extract 28 28) (select state1 CPSR)) #b0))
(and (= cond1 HI) (and (= ((_ extract 29 29) (select state1 CPSR)) #b1) (= ((_ extract 30 30) (select state1 CPSR)) #b0) ))
(and (= cond1 LS) (or (= ((_ extract 29 29) (select state1 CPSR)) #b0) (= ((_ extract 30 30) (select state1 CPSR)) #b1) ))
(and (= cond1 GE) (and (= ((_ extract 31 31) (select state1 CPSR)) #b1) (= ((_ extract 28 28) (select state1 CPSR)) #b1) ) )
(and (= cond1 LT) (or (= ((_ extract 31 31) (select state1 CPSR)) #b0) (= ((_ extract 28 28) (select state1 CPSR)) #b0) ) )
(and 
(= cond1 GT)
(and 
(= ((_ extract 30 30) (select state1 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state1 CPSR)) #b1) (= ((_ extract 28 28) (select state1 CPSR)) #b1)) 
)
)
(and 
(= cond1 LE)
(or 
(= ((_ extract 30 30) (select state1 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state1 CPSR)) #b1) (= ((_ extract 28 28) (select state1 CPSR)) #b1))) )
)
(= cond1 AL)
)
) 
; R0 to CPSR are equal
(= (select state1 R0) (select state2 R0)) 
(= (select state1 R1) (select state2 R1))
(= (select state1 R2) (select state2 R2))
(= (select state1 R3) (select state2 R3))
(= (select state1 R4) (select state2 R4))
(= (select state1 R5) (select state2 R5))
(= (select state1 R6) (select state2 R6))
(= (select state1 R7) (select state2 R7))
(= (select state1 R8) (select state2 R8))
(= (select state1 R9) (select state2 R9))
(= (select state1 R10) (select state2 R10))
(= (select state1 R11) (select state2 R11))
(= (select state1 R12) (select state2 R12))
(= (select state1 SP) (select state2 SP))
(= (select state1 LR) (select state2 LR)) 
(= (select state1 CPSR) (select state2 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm1_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm1))) (select state1 ro1))))
(ite
(= barrel_op1 LSL)
(bvshl val_to_shift barrel_num1)
(ite 
(= barrel_op1 LSR)
(bvlshr val_to_shift barrel_num1)
(bvashr val_to_shift barrel_num1)
)
)
)
)
(rd1_val (select state2 rd1)) (rn1_val (select state1 rn1))
)
; instruction is executed
(and
; cond1ition is true
(or
(and (= cond1 EQ) (= ((_ extract 30 30) (select state1 CPSR)) #b1))
(and (= cond1 NE) (= ((_ extract 30 30) (select state1 CPSR)) #b0))
(and (= cond1 CS) (= ((_ extract 29 29) (select state1 CPSR)) #b1))
(and (= cond1 CC) (= ((_ extract 29 29) (select state1 CPSR)) #b0))
(and (= cond1 MI) (= ((_ extract 31 31) (select state1 CPSR)) #b1))
(and (= cond1 PL) (= ((_ extract 31 31) (select state1 CPSR)) #b0))
(and (= cond1 VS) (= ((_ extract 28 28) (select state1 CPSR)) #b1))
(and (= cond1 VC) (= ((_ extract 28 28) (select state1 CPSR)) #b0))
(and (= cond1 HI) (and (= ((_ extract 29 29) (select state1 CPSR)) #b1) (= ((_ extract 30 30) (select state1 CPSR)) #b0) ))
(and (= cond1 LS) (or (= ((_ extract 29 29) (select state1 CPSR)) #b0) (= ((_ extract 30 30) (select state1 CPSR)) #b1) ))
(and (= cond1 GE) (and (= ((_ extract 31 31) (select state1 CPSR)) #b1) (= ((_ extract 28 28) (select state1 CPSR)) #b1) ) )
(and (= cond1 LT) (or (= ((_ extract 31 31) (select state1 CPSR)) #b0) (= ((_ extract 28 28) (select state1 CPSR)) #b0) ) )
(and 
(= cond1 GT) 
(and 
(= ((_ extract 30 30) (select state1 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state1 CPSR)) #b1) (= ((_ extract 28 28) (select state1 CPSR)) #b1)) 
)
)
(and 
(= cond1 LE)
(or 
(= ((_ extract 30 30) (select state1 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state1 CPSR)) #b1) (= ((_ extract 28 28) (select state1 CPSR)) #b1))) )
)
(= cond1 AL)
)
(or
; execute_compare
(and
(or (= oper1 CMP)) 
; R0 to LR are equal
(= (select state1 R0) (select state2 R0)) 
(= (select state1 R1) (select state2 R1))
(= (select state1 R2) (select state2 R2))
(= (select state1 R3) (select state2 R3))
(= (select state1 R4) (select state2 R4))
(= (select state1 R5) (select state2 R5))
(= (select state1 R6) (select state2 R6))
(= (select state1 R7) (select state2 R7))
(= (select state1 R8) (select state2 R8))
(= (select state1 R9) (select state2 R9))
(= (select state1 R10) (select state2 R10))
(= (select state1 R11) (select state2 R11))
(= (select state1 R12) (select state2 R12))
(= (select state1 SP) (select state2 SP))
(= (select state1 LR) (select state2 LR))
(or (not (= oper1 CMP)) 
(ite (= rn1_val flex_val)
(= ((_ extract 30 30) (select state2 CPSR)) #b1) ; Z flag1 is 1 
(= ((_ extract 30 30) (select state2 CPSR)) #b0) ; Z flag1 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state1 R0) (select state2 R0)) (= rd1 R0))  
(or (= (select state1 R1) (select state2 R1)) (= rd1 R1))
(or (= (select state1 R2) (select state2 R2)) (= rd1 R2))
(or (= (select state1 R3) (select state2 R3)) (= rd1 R3))
(or (= (select state1 R4) (select state2 R4)) (= rd1 R4))
(or (= (select state1 R5) (select state2 R5)) (= rd1 R5))
(or (= (select state1 R6) (select state2 R6)) (= rd1 R6))
(or (= (select state1 R7) (select state2 R7)) (= rd1 R7))
(or (= (select state1 R8) (select state2 R8)) (= rd1 R8))
(or (= (select state1 R9) (select state2 R9)) (= rd1 R9))
(or (= (select state1 R10) (select state2 R10)) (= rd1 R10))
(or (= (select state1 R11) (select state2 R11)) (= rd1 R11))
(or (= (select state1 R12) (select state2 R12)) (= rd1 R12))
(or (= (select state1 SP) (select state2 SP)) (= rd1 SP))
(or (= (select state1 LR) (select state2 LR)) (= rd1 LR))
(or
; execute_2
(and
(or (= oper1 MOV) (= oper1 MVN))
(or (not (= oper1 MOV)) (= rd1_val flex_val))
(or (not (= oper1 MVN)) (= rd1_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper1 ADD) (= oper1 SUB) (= oper1 AND)) 
(or (not (= oper1 ADD)) (= rd1_val (bvadd rn1_val flex_val)))
(or (not (= oper1 SUB)) (= rd1_val (bvsub rn1_val flex_val)))
(or (not (= oper1 AND)) (= rd1_val (bvand rn1_val flex_val)))
)
)
; set flag1s or not
(or 
(and (= flag1 N) (= (select state1 CPSR) (select state2 CPSR))) 
(and (= flag1 S) 
(ite 
(= (select state2 rd1) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state2 CPSR)) #b1) ;Z flag1 is 1
(= ((_ extract 30 30) (select state2 CPSR)) #b0) ;Z flag1 is 0
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
(assert (=> (transition state2 state3)
(and
; pc incremented
(= (select state3 PC) (bvadd (select state2 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond2ition is false
(not
(or
(and (= cond2 EQ) (= ((_ extract 30 30) (select state2 CPSR)) #b1))
(and (= cond2 NE) (= ((_ extract 30 30) (select state2 CPSR)) #b0))
(and (= cond2 CS) (= ((_ extract 29 29) (select state2 CPSR)) #b1))
(and (= cond2 CC) (= ((_ extract 29 29) (select state2 CPSR)) #b0))
(and (= cond2 MI) (= ((_ extract 31 31) (select state2 CPSR)) #b1))
(and (= cond2 PL) (= ((_ extract 31 31) (select state2 CPSR)) #b0))
(and (= cond2 VS) (= ((_ extract 28 28) (select state2 CPSR)) #b1))
(and (= cond2 VC) (= ((_ extract 28 28) (select state2 CPSR)) #b0))
(and (= cond2 HI) (and (= ((_ extract 29 29) (select state2 CPSR)) #b1) (= ((_ extract 30 30) (select state2 CPSR)) #b0) ))
(and (= cond2 LS) (or (= ((_ extract 29 29) (select state2 CPSR)) #b0) (= ((_ extract 30 30) (select state2 CPSR)) #b1) ))
(and (= cond2 GE) (and (= ((_ extract 31 31) (select state2 CPSR)) #b1) (= ((_ extract 28 28) (select state2 CPSR)) #b1) ) )
(and (= cond2 LT) (or (= ((_ extract 31 31) (select state2 CPSR)) #b0) (= ((_ extract 28 28) (select state2 CPSR)) #b0) ) )
(and 
(= cond2 GT)
(and 
(= ((_ extract 30 30) (select state2 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state2 CPSR)) #b1) (= ((_ extract 28 28) (select state2 CPSR)) #b1)) 
)
)
(and 
(= cond2 LE)
(or 
(= ((_ extract 30 30) (select state2 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state2 CPSR)) #b1) (= ((_ extract 28 28) (select state2 CPSR)) #b1))) )
)
(= cond2 AL)
)
) 
; R0 to CPSR are equal
(= (select state2 R0) (select state3 R0)) 
(= (select state2 R1) (select state3 R1))
(= (select state2 R2) (select state3 R2))
(= (select state2 R3) (select state3 R3))
(= (select state2 R4) (select state3 R4))
(= (select state2 R5) (select state3 R5))
(= (select state2 R6) (select state3 R6))
(= (select state2 R7) (select state3 R7))
(= (select state2 R8) (select state3 R8))
(= (select state2 R9) (select state3 R9))
(= (select state2 R10) (select state3 R10))
(= (select state2 R11) (select state3 R11))
(= (select state2 R12) (select state3 R12))
(= (select state2 SP) (select state3 SP))
(= (select state2 LR) (select state3 LR)) 
(= (select state2 CPSR) (select state3 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm2_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm2))) (select state2 ro2))))
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
(rd2_val (select state3 rd2)) (rn2_val (select state2 rn2))
)
; instruction is executed
(and
; cond2ition is true
(or
(and (= cond2 EQ) (= ((_ extract 30 30) (select state2 CPSR)) #b1))
(and (= cond2 NE) (= ((_ extract 30 30) (select state2 CPSR)) #b0))
(and (= cond2 CS) (= ((_ extract 29 29) (select state2 CPSR)) #b1))
(and (= cond2 CC) (= ((_ extract 29 29) (select state2 CPSR)) #b0))
(and (= cond2 MI) (= ((_ extract 31 31) (select state2 CPSR)) #b1))
(and (= cond2 PL) (= ((_ extract 31 31) (select state2 CPSR)) #b0))
(and (= cond2 VS) (= ((_ extract 28 28) (select state2 CPSR)) #b1))
(and (= cond2 VC) (= ((_ extract 28 28) (select state2 CPSR)) #b0))
(and (= cond2 HI) (and (= ((_ extract 29 29) (select state2 CPSR)) #b1) (= ((_ extract 30 30) (select state2 CPSR)) #b0) ))
(and (= cond2 LS) (or (= ((_ extract 29 29) (select state2 CPSR)) #b0) (= ((_ extract 30 30) (select state2 CPSR)) #b1) ))
(and (= cond2 GE) (and (= ((_ extract 31 31) (select state2 CPSR)) #b1) (= ((_ extract 28 28) (select state2 CPSR)) #b1) ) )
(and (= cond2 LT) (or (= ((_ extract 31 31) (select state2 CPSR)) #b0) (= ((_ extract 28 28) (select state2 CPSR)) #b0) ) )
(and 
(= cond2 GT) 
(and 
(= ((_ extract 30 30) (select state2 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state2 CPSR)) #b1) (= ((_ extract 28 28) (select state2 CPSR)) #b1)) 
)
)
(and 
(= cond2 LE)
(or 
(= ((_ extract 30 30) (select state2 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state2 CPSR)) #b1) (= ((_ extract 28 28) (select state2 CPSR)) #b1))) )
)
(= cond2 AL)
)
(or
; execute_compare
(and
(or (= oper2 CMP)) 
; R0 to LR are equal
(= (select state2 R0) (select state3 R0)) 
(= (select state2 R1) (select state3 R1))
(= (select state2 R2) (select state3 R2))
(= (select state2 R3) (select state3 R3))
(= (select state2 R4) (select state3 R4))
(= (select state2 R5) (select state3 R5))
(= (select state2 R6) (select state3 R6))
(= (select state2 R7) (select state3 R7))
(= (select state2 R8) (select state3 R8))
(= (select state2 R9) (select state3 R9))
(= (select state2 R10) (select state3 R10))
(= (select state2 R11) (select state3 R11))
(= (select state2 R12) (select state3 R12))
(= (select state2 SP) (select state3 SP))
(= (select state2 LR) (select state3 LR))
(or (not (= oper2 CMP)) 
(ite (= rn2_val flex_val)
(= ((_ extract 30 30) (select state3 CPSR)) #b1) ; Z flag2 is 1 
(= ((_ extract 30 30) (select state3 CPSR)) #b0) ; Z flag2 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state2 R0) (select state3 R0)) (= rd2 R0))  
(or (= (select state2 R1) (select state3 R1)) (= rd2 R1))
(or (= (select state2 R2) (select state3 R2)) (= rd2 R2))
(or (= (select state2 R3) (select state3 R3)) (= rd2 R3))
(or (= (select state2 R4) (select state3 R4)) (= rd2 R4))
(or (= (select state2 R5) (select state3 R5)) (= rd2 R5))
(or (= (select state2 R6) (select state3 R6)) (= rd2 R6))
(or (= (select state2 R7) (select state3 R7)) (= rd2 R7))
(or (= (select state2 R8) (select state3 R8)) (= rd2 R8))
(or (= (select state2 R9) (select state3 R9)) (= rd2 R9))
(or (= (select state2 R10) (select state3 R10)) (= rd2 R10))
(or (= (select state2 R11) (select state3 R11)) (= rd2 R11))
(or (= (select state2 R12) (select state3 R12)) (= rd2 R12))
(or (= (select state2 SP) (select state3 SP)) (= rd2 SP))
(or (= (select state2 LR) (select state3 LR)) (= rd2 LR))
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
(and (= flag2 N) (= (select state2 CPSR) (select state3 CPSR))) 
(and (= flag2 S) 
(ite 
(= (select state3 rd2) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state3 CPSR)) #b1) ;Z flag2 is 1
(= ((_ extract 30 30) (select state3 CPSR)) #b0) ;Z flag2 is 0
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
(assert (=> (transition state3 state4)
(and
; pc incremented
(= (select state4 PC) (bvadd (select state3 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond3ition is false
(not
(or
(and (= cond3 EQ) (= ((_ extract 30 30) (select state3 CPSR)) #b1))
(and (= cond3 NE) (= ((_ extract 30 30) (select state3 CPSR)) #b0))
(and (= cond3 CS) (= ((_ extract 29 29) (select state3 CPSR)) #b1))
(and (= cond3 CC) (= ((_ extract 29 29) (select state3 CPSR)) #b0))
(and (= cond3 MI) (= ((_ extract 31 31) (select state3 CPSR)) #b1))
(and (= cond3 PL) (= ((_ extract 31 31) (select state3 CPSR)) #b0))
(and (= cond3 VS) (= ((_ extract 28 28) (select state3 CPSR)) #b1))
(and (= cond3 VC) (= ((_ extract 28 28) (select state3 CPSR)) #b0))
(and (= cond3 HI) (and (= ((_ extract 29 29) (select state3 CPSR)) #b1) (= ((_ extract 30 30) (select state3 CPSR)) #b0) ))
(and (= cond3 LS) (or (= ((_ extract 29 29) (select state3 CPSR)) #b0) (= ((_ extract 30 30) (select state3 CPSR)) #b1) ))
(and (= cond3 GE) (and (= ((_ extract 31 31) (select state3 CPSR)) #b1) (= ((_ extract 28 28) (select state3 CPSR)) #b1) ) )
(and (= cond3 LT) (or (= ((_ extract 31 31) (select state3 CPSR)) #b0) (= ((_ extract 28 28) (select state3 CPSR)) #b0) ) )
(and 
(= cond3 GT)
(and 
(= ((_ extract 30 30) (select state3 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state3 CPSR)) #b1) (= ((_ extract 28 28) (select state3 CPSR)) #b1)) 
)
)
(and 
(= cond3 LE)
(or 
(= ((_ extract 30 30) (select state3 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state3 CPSR)) #b1) (= ((_ extract 28 28) (select state3 CPSR)) #b1))) )
)
(= cond3 AL)
)
) 
; R0 to CPSR are equal
(= (select state3 R0) (select state4 R0)) 
(= (select state3 R1) (select state4 R1))
(= (select state3 R2) (select state4 R2))
(= (select state3 R3) (select state4 R3))
(= (select state3 R4) (select state4 R4))
(= (select state3 R5) (select state4 R5))
(= (select state3 R6) (select state4 R6))
(= (select state3 R7) (select state4 R7))
(= (select state3 R8) (select state4 R8))
(= (select state3 R9) (select state4 R9))
(= (select state3 R10) (select state4 R10))
(= (select state3 R11) (select state4 R11))
(= (select state3 R12) (select state4 R12))
(= (select state3 SP) (select state4 SP))
(= (select state3 LR) (select state4 LR)) 
(= (select state3 CPSR) (select state4 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm3_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm3))) (select state3 ro3))))
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
(rd3_val (select state4 rd3)) (rn3_val (select state3 rn3))
)
; instruction is executed
(and
; cond3ition is true
(or
(and (= cond3 EQ) (= ((_ extract 30 30) (select state3 CPSR)) #b1))
(and (= cond3 NE) (= ((_ extract 30 30) (select state3 CPSR)) #b0))
(and (= cond3 CS) (= ((_ extract 29 29) (select state3 CPSR)) #b1))
(and (= cond3 CC) (= ((_ extract 29 29) (select state3 CPSR)) #b0))
(and (= cond3 MI) (= ((_ extract 31 31) (select state3 CPSR)) #b1))
(and (= cond3 PL) (= ((_ extract 31 31) (select state3 CPSR)) #b0))
(and (= cond3 VS) (= ((_ extract 28 28) (select state3 CPSR)) #b1))
(and (= cond3 VC) (= ((_ extract 28 28) (select state3 CPSR)) #b0))
(and (= cond3 HI) (and (= ((_ extract 29 29) (select state3 CPSR)) #b1) (= ((_ extract 30 30) (select state3 CPSR)) #b0) ))
(and (= cond3 LS) (or (= ((_ extract 29 29) (select state3 CPSR)) #b0) (= ((_ extract 30 30) (select state3 CPSR)) #b1) ))
(and (= cond3 GE) (and (= ((_ extract 31 31) (select state3 CPSR)) #b1) (= ((_ extract 28 28) (select state3 CPSR)) #b1) ) )
(and (= cond3 LT) (or (= ((_ extract 31 31) (select state3 CPSR)) #b0) (= ((_ extract 28 28) (select state3 CPSR)) #b0) ) )
(and 
(= cond3 GT) 
(and 
(= ((_ extract 30 30) (select state3 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state3 CPSR)) #b1) (= ((_ extract 28 28) (select state3 CPSR)) #b1)) 
)
)
(and 
(= cond3 LE)
(or 
(= ((_ extract 30 30) (select state3 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state3 CPSR)) #b1) (= ((_ extract 28 28) (select state3 CPSR)) #b1))) )
)
(= cond3 AL)
)
(or
; execute_compare
(and
(or (= oper3 CMP)) 
; R0 to LR are equal
(= (select state3 R0) (select state4 R0)) 
(= (select state3 R1) (select state4 R1))
(= (select state3 R2) (select state4 R2))
(= (select state3 R3) (select state4 R3))
(= (select state3 R4) (select state4 R4))
(= (select state3 R5) (select state4 R5))
(= (select state3 R6) (select state4 R6))
(= (select state3 R7) (select state4 R7))
(= (select state3 R8) (select state4 R8))
(= (select state3 R9) (select state4 R9))
(= (select state3 R10) (select state4 R10))
(= (select state3 R11) (select state4 R11))
(= (select state3 R12) (select state4 R12))
(= (select state3 SP) (select state4 SP))
(= (select state3 LR) (select state4 LR))
(or (not (= oper3 CMP)) 
(ite (= rn3_val flex_val)
(= ((_ extract 30 30) (select state4 CPSR)) #b1) ; Z flag3 is 1 
(= ((_ extract 30 30) (select state4 CPSR)) #b0) ; Z flag3 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state3 R0) (select state4 R0)) (= rd3 R0))  
(or (= (select state3 R1) (select state4 R1)) (= rd3 R1))
(or (= (select state3 R2) (select state4 R2)) (= rd3 R2))
(or (= (select state3 R3) (select state4 R3)) (= rd3 R3))
(or (= (select state3 R4) (select state4 R4)) (= rd3 R4))
(or (= (select state3 R5) (select state4 R5)) (= rd3 R5))
(or (= (select state3 R6) (select state4 R6)) (= rd3 R6))
(or (= (select state3 R7) (select state4 R7)) (= rd3 R7))
(or (= (select state3 R8) (select state4 R8)) (= rd3 R8))
(or (= (select state3 R9) (select state4 R9)) (= rd3 R9))
(or (= (select state3 R10) (select state4 R10)) (= rd3 R10))
(or (= (select state3 R11) (select state4 R11)) (= rd3 R11))
(or (= (select state3 R12) (select state4 R12)) (= rd3 R12))
(or (= (select state3 SP) (select state4 SP)) (= rd3 SP))
(or (= (select state3 LR) (select state4 LR)) (= rd3 LR))
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
(and (= flag3 N) (= (select state3 CPSR) (select state4 CPSR))) 
(and (= flag3 S) 
(ite 
(= (select state4 rd3) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state4 CPSR)) #b1) ;Z flag3 is 1
(= ((_ extract 30 30) (select state4 CPSR)) #b0) ;Z flag3 is 0
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
(assert (=> (transition state4 state5)
(and
; pc incremented
(= (select state5 PC) (bvadd (select state4 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond4ition is false
(not
(or
(and (= cond4 EQ) (= ((_ extract 30 30) (select state4 CPSR)) #b1))
(and (= cond4 NE) (= ((_ extract 30 30) (select state4 CPSR)) #b0))
(and (= cond4 CS) (= ((_ extract 29 29) (select state4 CPSR)) #b1))
(and (= cond4 CC) (= ((_ extract 29 29) (select state4 CPSR)) #b0))
(and (= cond4 MI) (= ((_ extract 31 31) (select state4 CPSR)) #b1))
(and (= cond4 PL) (= ((_ extract 31 31) (select state4 CPSR)) #b0))
(and (= cond4 VS) (= ((_ extract 28 28) (select state4 CPSR)) #b1))
(and (= cond4 VC) (= ((_ extract 28 28) (select state4 CPSR)) #b0))
(and (= cond4 HI) (and (= ((_ extract 29 29) (select state4 CPSR)) #b1) (= ((_ extract 30 30) (select state4 CPSR)) #b0) ))
(and (= cond4 LS) (or (= ((_ extract 29 29) (select state4 CPSR)) #b0) (= ((_ extract 30 30) (select state4 CPSR)) #b1) ))
(and (= cond4 GE) (and (= ((_ extract 31 31) (select state4 CPSR)) #b1) (= ((_ extract 28 28) (select state4 CPSR)) #b1) ) )
(and (= cond4 LT) (or (= ((_ extract 31 31) (select state4 CPSR)) #b0) (= ((_ extract 28 28) (select state4 CPSR)) #b0) ) )
(and 
(= cond4 GT)
(and 
(= ((_ extract 30 30) (select state4 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state4 CPSR)) #b1) (= ((_ extract 28 28) (select state4 CPSR)) #b1)) 
)
)
(and 
(= cond4 LE)
(or 
(= ((_ extract 30 30) (select state4 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state4 CPSR)) #b1) (= ((_ extract 28 28) (select state4 CPSR)) #b1))) )
)
(= cond4 AL)
)
) 
; R0 to CPSR are equal
(= (select state4 R0) (select state5 R0)) 
(= (select state4 R1) (select state5 R1))
(= (select state4 R2) (select state5 R2))
(= (select state4 R3) (select state5 R3))
(= (select state4 R4) (select state5 R4))
(= (select state4 R5) (select state5 R5))
(= (select state4 R6) (select state5 R6))
(= (select state4 R7) (select state5 R7))
(= (select state4 R8) (select state5 R8))
(= (select state4 R9) (select state5 R9))
(= (select state4 R10) (select state5 R10))
(= (select state4 R11) (select state5 R11))
(= (select state4 R12) (select state5 R12))
(= (select state4 SP) (select state5 SP))
(= (select state4 LR) (select state5 LR)) 
(= (select state4 CPSR) (select state5 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm4_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm4))) (select state4 ro4))))
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
(rd4_val (select state5 rd4)) (rn4_val (select state4 rn4))
)
; instruction is executed
(and
; cond4ition is true
(or
(and (= cond4 EQ) (= ((_ extract 30 30) (select state4 CPSR)) #b1))
(and (= cond4 NE) (= ((_ extract 30 30) (select state4 CPSR)) #b0))
(and (= cond4 CS) (= ((_ extract 29 29) (select state4 CPSR)) #b1))
(and (= cond4 CC) (= ((_ extract 29 29) (select state4 CPSR)) #b0))
(and (= cond4 MI) (= ((_ extract 31 31) (select state4 CPSR)) #b1))
(and (= cond4 PL) (= ((_ extract 31 31) (select state4 CPSR)) #b0))
(and (= cond4 VS) (= ((_ extract 28 28) (select state4 CPSR)) #b1))
(and (= cond4 VC) (= ((_ extract 28 28) (select state4 CPSR)) #b0))
(and (= cond4 HI) (and (= ((_ extract 29 29) (select state4 CPSR)) #b1) (= ((_ extract 30 30) (select state4 CPSR)) #b0) ))
(and (= cond4 LS) (or (= ((_ extract 29 29) (select state4 CPSR)) #b0) (= ((_ extract 30 30) (select state4 CPSR)) #b1) ))
(and (= cond4 GE) (and (= ((_ extract 31 31) (select state4 CPSR)) #b1) (= ((_ extract 28 28) (select state4 CPSR)) #b1) ) )
(and (= cond4 LT) (or (= ((_ extract 31 31) (select state4 CPSR)) #b0) (= ((_ extract 28 28) (select state4 CPSR)) #b0) ) )
(and 
(= cond4 GT) 
(and 
(= ((_ extract 30 30) (select state4 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state4 CPSR)) #b1) (= ((_ extract 28 28) (select state4 CPSR)) #b1)) 
)
)
(and 
(= cond4 LE)
(or 
(= ((_ extract 30 30) (select state4 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state4 CPSR)) #b1) (= ((_ extract 28 28) (select state4 CPSR)) #b1))) )
)
(= cond4 AL)
)
(or
; execute_compare
(and
(or (= oper4 CMP)) 
; R0 to LR are equal
(= (select state4 R0) (select state5 R0)) 
(= (select state4 R1) (select state5 R1))
(= (select state4 R2) (select state5 R2))
(= (select state4 R3) (select state5 R3))
(= (select state4 R4) (select state5 R4))
(= (select state4 R5) (select state5 R5))
(= (select state4 R6) (select state5 R6))
(= (select state4 R7) (select state5 R7))
(= (select state4 R8) (select state5 R8))
(= (select state4 R9) (select state5 R9))
(= (select state4 R10) (select state5 R10))
(= (select state4 R11) (select state5 R11))
(= (select state4 R12) (select state5 R12))
(= (select state4 SP) (select state5 SP))
(= (select state4 LR) (select state5 LR))
(or (not (= oper4 CMP)) 
(ite (= rn4_val flex_val)
(= ((_ extract 30 30) (select state5 CPSR)) #b1) ; Z flag4 is 1 
(= ((_ extract 30 30) (select state5 CPSR)) #b0) ; Z flag4 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state4 R0) (select state5 R0)) (= rd4 R0))  
(or (= (select state4 R1) (select state5 R1)) (= rd4 R1))
(or (= (select state4 R2) (select state5 R2)) (= rd4 R2))
(or (= (select state4 R3) (select state5 R3)) (= rd4 R3))
(or (= (select state4 R4) (select state5 R4)) (= rd4 R4))
(or (= (select state4 R5) (select state5 R5)) (= rd4 R5))
(or (= (select state4 R6) (select state5 R6)) (= rd4 R6))
(or (= (select state4 R7) (select state5 R7)) (= rd4 R7))
(or (= (select state4 R8) (select state5 R8)) (= rd4 R8))
(or (= (select state4 R9) (select state5 R9)) (= rd4 R9))
(or (= (select state4 R10) (select state5 R10)) (= rd4 R10))
(or (= (select state4 R11) (select state5 R11)) (= rd4 R11))
(or (= (select state4 R12) (select state5 R12)) (= rd4 R12))
(or (= (select state4 SP) (select state5 SP)) (= rd4 SP))
(or (= (select state4 LR) (select state5 LR)) (= rd4 LR))
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
; set flag4s or not
(or 
(and (= flag4 N) (= (select state4 CPSR) (select state5 CPSR))) 
(and (= flag4 S) 
(ite 
(= (select state5 rd4) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state5 CPSR)) #b1) ;Z flag4 is 1
(= ((_ extract 30 30) (select state5 CPSR)) #b0) ;Z flag4 is 0
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
(assert (=> (transition state5 state6)
(and
; pc incremented
(= (select state6 PC) (bvadd (select state5 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond5ition is false
(not
(or
(and (= cond5 EQ) (= ((_ extract 30 30) (select state5 CPSR)) #b1))
(and (= cond5 NE) (= ((_ extract 30 30) (select state5 CPSR)) #b0))
(and (= cond5 CS) (= ((_ extract 29 29) (select state5 CPSR)) #b1))
(and (= cond5 CC) (= ((_ extract 29 29) (select state5 CPSR)) #b0))
(and (= cond5 MI) (= ((_ extract 31 31) (select state5 CPSR)) #b1))
(and (= cond5 PL) (= ((_ extract 31 31) (select state5 CPSR)) #b0))
(and (= cond5 VS) (= ((_ extract 28 28) (select state5 CPSR)) #b1))
(and (= cond5 VC) (= ((_ extract 28 28) (select state5 CPSR)) #b0))
(and (= cond5 HI) (and (= ((_ extract 29 29) (select state5 CPSR)) #b1) (= ((_ extract 30 30) (select state5 CPSR)) #b0) ))
(and (= cond5 LS) (or (= ((_ extract 29 29) (select state5 CPSR)) #b0) (= ((_ extract 30 30) (select state5 CPSR)) #b1) ))
(and (= cond5 GE) (and (= ((_ extract 31 31) (select state5 CPSR)) #b1) (= ((_ extract 28 28) (select state5 CPSR)) #b1) ) )
(and (= cond5 LT) (or (= ((_ extract 31 31) (select state5 CPSR)) #b0) (= ((_ extract 28 28) (select state5 CPSR)) #b0) ) )
(and 
(= cond5 GT)
(and 
(= ((_ extract 30 30) (select state5 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state5 CPSR)) #b1) (= ((_ extract 28 28) (select state5 CPSR)) #b1)) 
)
)
(and 
(= cond5 LE)
(or 
(= ((_ extract 30 30) (select state5 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state5 CPSR)) #b1) (= ((_ extract 28 28) (select state5 CPSR)) #b1))) )
)
(= cond5 AL)
)
) 
; R0 to CPSR are equal
(= (select state5 R0) (select state6 R0)) 
(= (select state5 R1) (select state6 R1))
(= (select state5 R2) (select state6 R2))
(= (select state5 R3) (select state6 R3))
(= (select state5 R4) (select state6 R4))
(= (select state5 R5) (select state6 R5))
(= (select state5 R6) (select state6 R6))
(= (select state5 R7) (select state6 R7))
(= (select state5 R8) (select state6 R8))
(= (select state5 R9) (select state6 R9))
(= (select state5 R10) (select state6 R10))
(= (select state5 R11) (select state6 R11))
(= (select state5 R12) (select state6 R12))
(= (select state5 SP) (select state6 SP))
(= (select state5 LR) (select state6 LR)) 
(= (select state5 CPSR) (select state6 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm5_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm5))) (select state5 ro5))))
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
(rd5_val (select state6 rd5)) (rn5_val (select state5 rn5))
)
; instruction is executed
(and
; cond5ition is true
(or
(and (= cond5 EQ) (= ((_ extract 30 30) (select state5 CPSR)) #b1))
(and (= cond5 NE) (= ((_ extract 30 30) (select state5 CPSR)) #b0))
(and (= cond5 CS) (= ((_ extract 29 29) (select state5 CPSR)) #b1))
(and (= cond5 CC) (= ((_ extract 29 29) (select state5 CPSR)) #b0))
(and (= cond5 MI) (= ((_ extract 31 31) (select state5 CPSR)) #b1))
(and (= cond5 PL) (= ((_ extract 31 31) (select state5 CPSR)) #b0))
(and (= cond5 VS) (= ((_ extract 28 28) (select state5 CPSR)) #b1))
(and (= cond5 VC) (= ((_ extract 28 28) (select state5 CPSR)) #b0))
(and (= cond5 HI) (and (= ((_ extract 29 29) (select state5 CPSR)) #b1) (= ((_ extract 30 30) (select state5 CPSR)) #b0) ))
(and (= cond5 LS) (or (= ((_ extract 29 29) (select state5 CPSR)) #b0) (= ((_ extract 30 30) (select state5 CPSR)) #b1) ))
(and (= cond5 GE) (and (= ((_ extract 31 31) (select state5 CPSR)) #b1) (= ((_ extract 28 28) (select state5 CPSR)) #b1) ) )
(and (= cond5 LT) (or (= ((_ extract 31 31) (select state5 CPSR)) #b0) (= ((_ extract 28 28) (select state5 CPSR)) #b0) ) )
(and 
(= cond5 GT) 
(and 
(= ((_ extract 30 30) (select state5 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state5 CPSR)) #b1) (= ((_ extract 28 28) (select state5 CPSR)) #b1)) 
)
)
(and 
(= cond5 LE)
(or 
(= ((_ extract 30 30) (select state5 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state5 CPSR)) #b1) (= ((_ extract 28 28) (select state5 CPSR)) #b1))) )
)
(= cond5 AL)
)
(or
; execute_compare
(and
(or (= oper5 CMP)) 
; R0 to LR are equal
(= (select state5 R0) (select state6 R0)) 
(= (select state5 R1) (select state6 R1))
(= (select state5 R2) (select state6 R2))
(= (select state5 R3) (select state6 R3))
(= (select state5 R4) (select state6 R4))
(= (select state5 R5) (select state6 R5))
(= (select state5 R6) (select state6 R6))
(= (select state5 R7) (select state6 R7))
(= (select state5 R8) (select state6 R8))
(= (select state5 R9) (select state6 R9))
(= (select state5 R10) (select state6 R10))
(= (select state5 R11) (select state6 R11))
(= (select state5 R12) (select state6 R12))
(= (select state5 SP) (select state6 SP))
(= (select state5 LR) (select state6 LR))
(or (not (= oper5 CMP)) 
(ite (= rn5_val flex_val)
(= ((_ extract 30 30) (select state6 CPSR)) #b1) ; Z flag5 is 1 
(= ((_ extract 30 30) (select state6 CPSR)) #b0) ; Z flag5 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state5 R0) (select state6 R0)) (= rd5 R0))  
(or (= (select state5 R1) (select state6 R1)) (= rd5 R1))
(or (= (select state5 R2) (select state6 R2)) (= rd5 R2))
(or (= (select state5 R3) (select state6 R3)) (= rd5 R3))
(or (= (select state5 R4) (select state6 R4)) (= rd5 R4))
(or (= (select state5 R5) (select state6 R5)) (= rd5 R5))
(or (= (select state5 R6) (select state6 R6)) (= rd5 R6))
(or (= (select state5 R7) (select state6 R7)) (= rd5 R7))
(or (= (select state5 R8) (select state6 R8)) (= rd5 R8))
(or (= (select state5 R9) (select state6 R9)) (= rd5 R9))
(or (= (select state5 R10) (select state6 R10)) (= rd5 R10))
(or (= (select state5 R11) (select state6 R11)) (= rd5 R11))
(or (= (select state5 R12) (select state6 R12)) (= rd5 R12))
(or (= (select state5 SP) (select state6 SP)) (= rd5 SP))
(or (= (select state5 LR) (select state6 LR)) (= rd5 LR))
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
; set flag5s or not
(or 
(and (= flag5 N) (= (select state5 CPSR) (select state6 CPSR))) 
(and (= flag5 S) 
(ite 
(= (select state6 rd5) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state6 CPSR)) #b1) ;Z flag5 is 1
(= ((_ extract 30 30) (select state6 CPSR)) #b0) ;Z flag5 is 0
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
(assert (=> (transition state6 state7)
(and
; pc incremented
(= (select state7 PC) (bvadd (select state6 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond6ition is false
(not
(or
(and (= cond6 EQ) (= ((_ extract 30 30) (select state6 CPSR)) #b1))
(and (= cond6 NE) (= ((_ extract 30 30) (select state6 CPSR)) #b0))
(and (= cond6 CS) (= ((_ extract 29 29) (select state6 CPSR)) #b1))
(and (= cond6 CC) (= ((_ extract 29 29) (select state6 CPSR)) #b0))
(and (= cond6 MI) (= ((_ extract 31 31) (select state6 CPSR)) #b1))
(and (= cond6 PL) (= ((_ extract 31 31) (select state6 CPSR)) #b0))
(and (= cond6 VS) (= ((_ extract 28 28) (select state6 CPSR)) #b1))
(and (= cond6 VC) (= ((_ extract 28 28) (select state6 CPSR)) #b0))
(and (= cond6 HI) (and (= ((_ extract 29 29) (select state6 CPSR)) #b1) (= ((_ extract 30 30) (select state6 CPSR)) #b0) ))
(and (= cond6 LS) (or (= ((_ extract 29 29) (select state6 CPSR)) #b0) (= ((_ extract 30 30) (select state6 CPSR)) #b1) ))
(and (= cond6 GE) (and (= ((_ extract 31 31) (select state6 CPSR)) #b1) (= ((_ extract 28 28) (select state6 CPSR)) #b1) ) )
(and (= cond6 LT) (or (= ((_ extract 31 31) (select state6 CPSR)) #b0) (= ((_ extract 28 28) (select state6 CPSR)) #b0) ) )
(and 
(= cond6 GT)
(and 
(= ((_ extract 30 30) (select state6 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state6 CPSR)) #b1) (= ((_ extract 28 28) (select state6 CPSR)) #b1)) 
)
)
(and 
(= cond6 LE)
(or 
(= ((_ extract 30 30) (select state6 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state6 CPSR)) #b1) (= ((_ extract 28 28) (select state6 CPSR)) #b1))) )
)
(= cond6 AL)
)
) 
; R0 to CPSR are equal
(= (select state6 R0) (select state7 R0)) 
(= (select state6 R1) (select state7 R1))
(= (select state6 R2) (select state7 R2))
(= (select state6 R3) (select state7 R3))
(= (select state6 R4) (select state7 R4))
(= (select state6 R5) (select state7 R5))
(= (select state6 R6) (select state7 R6))
(= (select state6 R7) (select state7 R7))
(= (select state6 R8) (select state7 R8))
(= (select state6 R9) (select state7 R9))
(= (select state6 R10) (select state7 R10))
(= (select state6 R11) (select state7 R11))
(= (select state6 R12) (select state7 R12))
(= (select state6 SP) (select state7 SP))
(= (select state6 LR) (select state7 LR)) 
(= (select state6 CPSR) (select state7 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm6_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm6))) (select state6 ro6))))
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
(rd6_val (select state7 rd6)) (rn6_val (select state6 rn6))
)
; instruction is executed
(and
; cond6ition is true
(or
(and (= cond6 EQ) (= ((_ extract 30 30) (select state6 CPSR)) #b1))
(and (= cond6 NE) (= ((_ extract 30 30) (select state6 CPSR)) #b0))
(and (= cond6 CS) (= ((_ extract 29 29) (select state6 CPSR)) #b1))
(and (= cond6 CC) (= ((_ extract 29 29) (select state6 CPSR)) #b0))
(and (= cond6 MI) (= ((_ extract 31 31) (select state6 CPSR)) #b1))
(and (= cond6 PL) (= ((_ extract 31 31) (select state6 CPSR)) #b0))
(and (= cond6 VS) (= ((_ extract 28 28) (select state6 CPSR)) #b1))
(and (= cond6 VC) (= ((_ extract 28 28) (select state6 CPSR)) #b0))
(and (= cond6 HI) (and (= ((_ extract 29 29) (select state6 CPSR)) #b1) (= ((_ extract 30 30) (select state6 CPSR)) #b0) ))
(and (= cond6 LS) (or (= ((_ extract 29 29) (select state6 CPSR)) #b0) (= ((_ extract 30 30) (select state6 CPSR)) #b1) ))
(and (= cond6 GE) (and (= ((_ extract 31 31) (select state6 CPSR)) #b1) (= ((_ extract 28 28) (select state6 CPSR)) #b1) ) )
(and (= cond6 LT) (or (= ((_ extract 31 31) (select state6 CPSR)) #b0) (= ((_ extract 28 28) (select state6 CPSR)) #b0) ) )
(and 
(= cond6 GT) 
(and 
(= ((_ extract 30 30) (select state6 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state6 CPSR)) #b1) (= ((_ extract 28 28) (select state6 CPSR)) #b1)) 
)
)
(and 
(= cond6 LE)
(or 
(= ((_ extract 30 30) (select state6 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state6 CPSR)) #b1) (= ((_ extract 28 28) (select state6 CPSR)) #b1))) )
)
(= cond6 AL)
)
(or
; execute_compare
(and
(or (= oper6 CMP)) 
; R0 to LR are equal
(= (select state6 R0) (select state7 R0)) 
(= (select state6 R1) (select state7 R1))
(= (select state6 R2) (select state7 R2))
(= (select state6 R3) (select state7 R3))
(= (select state6 R4) (select state7 R4))
(= (select state6 R5) (select state7 R5))
(= (select state6 R6) (select state7 R6))
(= (select state6 R7) (select state7 R7))
(= (select state6 R8) (select state7 R8))
(= (select state6 R9) (select state7 R9))
(= (select state6 R10) (select state7 R10))
(= (select state6 R11) (select state7 R11))
(= (select state6 R12) (select state7 R12))
(= (select state6 SP) (select state7 SP))
(= (select state6 LR) (select state7 LR))
(or (not (= oper6 CMP)) 
(ite (= rn6_val flex_val)
(= ((_ extract 30 30) (select state7 CPSR)) #b1) ; Z flag6 is 1 
(= ((_ extract 30 30) (select state7 CPSR)) #b0) ; Z flag6 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state6 R0) (select state7 R0)) (= rd6 R0))  
(or (= (select state6 R1) (select state7 R1)) (= rd6 R1))
(or (= (select state6 R2) (select state7 R2)) (= rd6 R2))
(or (= (select state6 R3) (select state7 R3)) (= rd6 R3))
(or (= (select state6 R4) (select state7 R4)) (= rd6 R4))
(or (= (select state6 R5) (select state7 R5)) (= rd6 R5))
(or (= (select state6 R6) (select state7 R6)) (= rd6 R6))
(or (= (select state6 R7) (select state7 R7)) (= rd6 R7))
(or (= (select state6 R8) (select state7 R8)) (= rd6 R8))
(or (= (select state6 R9) (select state7 R9)) (= rd6 R9))
(or (= (select state6 R10) (select state7 R10)) (= rd6 R10))
(or (= (select state6 R11) (select state7 R11)) (= rd6 R11))
(or (= (select state6 R12) (select state7 R12)) (= rd6 R12))
(or (= (select state6 SP) (select state7 SP)) (= rd6 SP))
(or (= (select state6 LR) (select state7 LR)) (= rd6 LR))
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
; set flag6s or not
(or 
(and (= flag6 N) (= (select state6 CPSR) (select state7 CPSR))) 
(and (= flag6 S) 
(ite 
(= (select state7 rd6) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state7 CPSR)) #b1) ;Z flag6 is 1
(= ((_ extract 30 30) (select state7 CPSR)) #b0) ;Z flag6 is 0
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
(assert (=> (transition state7 state8)
(and
; pc incremented
(= (select state8 PC) (bvadd (select state7 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond7ition is false
(not
(or
(and (= cond7 EQ) (= ((_ extract 30 30) (select state7 CPSR)) #b1))
(and (= cond7 NE) (= ((_ extract 30 30) (select state7 CPSR)) #b0))
(and (= cond7 CS) (= ((_ extract 29 29) (select state7 CPSR)) #b1))
(and (= cond7 CC) (= ((_ extract 29 29) (select state7 CPSR)) #b0))
(and (= cond7 MI) (= ((_ extract 31 31) (select state7 CPSR)) #b1))
(and (= cond7 PL) (= ((_ extract 31 31) (select state7 CPSR)) #b0))
(and (= cond7 VS) (= ((_ extract 28 28) (select state7 CPSR)) #b1))
(and (= cond7 VC) (= ((_ extract 28 28) (select state7 CPSR)) #b0))
(and (= cond7 HI) (and (= ((_ extract 29 29) (select state7 CPSR)) #b1) (= ((_ extract 30 30) (select state7 CPSR)) #b0) ))
(and (= cond7 LS) (or (= ((_ extract 29 29) (select state7 CPSR)) #b0) (= ((_ extract 30 30) (select state7 CPSR)) #b1) ))
(and (= cond7 GE) (and (= ((_ extract 31 31) (select state7 CPSR)) #b1) (= ((_ extract 28 28) (select state7 CPSR)) #b1) ) )
(and (= cond7 LT) (or (= ((_ extract 31 31) (select state7 CPSR)) #b0) (= ((_ extract 28 28) (select state7 CPSR)) #b0) ) )
(and 
(= cond7 GT)
(and 
(= ((_ extract 30 30) (select state7 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state7 CPSR)) #b1) (= ((_ extract 28 28) (select state7 CPSR)) #b1)) 
)
)
(and 
(= cond7 LE)
(or 
(= ((_ extract 30 30) (select state7 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state7 CPSR)) #b1) (= ((_ extract 28 28) (select state7 CPSR)) #b1))) )
)
(= cond7 AL)
)
) 
; R0 to CPSR are equal
(= (select state7 R0) (select state8 R0)) 
(= (select state7 R1) (select state8 R1))
(= (select state7 R2) (select state8 R2))
(= (select state7 R3) (select state8 R3))
(= (select state7 R4) (select state8 R4))
(= (select state7 R5) (select state8 R5))
(= (select state7 R6) (select state8 R6))
(= (select state7 R7) (select state8 R7))
(= (select state7 R8) (select state8 R8))
(= (select state7 R9) (select state8 R9))
(= (select state7 R10) (select state8 R10))
(= (select state7 R11) (select state8 R11))
(= (select state7 R12) (select state8 R12))
(= (select state7 SP) (select state8 SP))
(= (select state7 LR) (select state8 LR)) 
(= (select state7 CPSR) (select state8 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm7_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm7))) (select state7 ro7))))
(ite
(= barrel_op7 LSL)
(bvshl val_to_shift barrel_num7)
(ite 
(= barrel_op7 LSR)
(bvlshr val_to_shift barrel_num7)
(bvashr val_to_shift barrel_num7)
)
)
)
)
(rd7_val (select state8 rd7)) (rn7_val (select state7 rn7))
)
; instruction is executed
(and
; cond7ition is true
(or
(and (= cond7 EQ) (= ((_ extract 30 30) (select state7 CPSR)) #b1))
(and (= cond7 NE) (= ((_ extract 30 30) (select state7 CPSR)) #b0))
(and (= cond7 CS) (= ((_ extract 29 29) (select state7 CPSR)) #b1))
(and (= cond7 CC) (= ((_ extract 29 29) (select state7 CPSR)) #b0))
(and (= cond7 MI) (= ((_ extract 31 31) (select state7 CPSR)) #b1))
(and (= cond7 PL) (= ((_ extract 31 31) (select state7 CPSR)) #b0))
(and (= cond7 VS) (= ((_ extract 28 28) (select state7 CPSR)) #b1))
(and (= cond7 VC) (= ((_ extract 28 28) (select state7 CPSR)) #b0))
(and (= cond7 HI) (and (= ((_ extract 29 29) (select state7 CPSR)) #b1) (= ((_ extract 30 30) (select state7 CPSR)) #b0) ))
(and (= cond7 LS) (or (= ((_ extract 29 29) (select state7 CPSR)) #b0) (= ((_ extract 30 30) (select state7 CPSR)) #b1) ))
(and (= cond7 GE) (and (= ((_ extract 31 31) (select state7 CPSR)) #b1) (= ((_ extract 28 28) (select state7 CPSR)) #b1) ) )
(and (= cond7 LT) (or (= ((_ extract 31 31) (select state7 CPSR)) #b0) (= ((_ extract 28 28) (select state7 CPSR)) #b0) ) )
(and 
(= cond7 GT) 
(and 
(= ((_ extract 30 30) (select state7 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state7 CPSR)) #b1) (= ((_ extract 28 28) (select state7 CPSR)) #b1)) 
)
)
(and 
(= cond7 LE)
(or 
(= ((_ extract 30 30) (select state7 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state7 CPSR)) #b1) (= ((_ extract 28 28) (select state7 CPSR)) #b1))) )
)
(= cond7 AL)
)
(or
; execute_compare
(and
(or (= oper7 CMP)) 
; R0 to LR are equal
(= (select state7 R0) (select state8 R0)) 
(= (select state7 R1) (select state8 R1))
(= (select state7 R2) (select state8 R2))
(= (select state7 R3) (select state8 R3))
(= (select state7 R4) (select state8 R4))
(= (select state7 R5) (select state8 R5))
(= (select state7 R6) (select state8 R6))
(= (select state7 R7) (select state8 R7))
(= (select state7 R8) (select state8 R8))
(= (select state7 R9) (select state8 R9))
(= (select state7 R10) (select state8 R10))
(= (select state7 R11) (select state8 R11))
(= (select state7 R12) (select state8 R12))
(= (select state7 SP) (select state8 SP))
(= (select state7 LR) (select state8 LR))
(or (not (= oper7 CMP)) 
(ite (= rn7_val flex_val)
(= ((_ extract 30 30) (select state8 CPSR)) #b1) ; Z flag7 is 1 
(= ((_ extract 30 30) (select state8 CPSR)) #b0) ; Z flag7 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state7 R0) (select state8 R0)) (= rd7 R0))  
(or (= (select state7 R1) (select state8 R1)) (= rd7 R1))
(or (= (select state7 R2) (select state8 R2)) (= rd7 R2))
(or (= (select state7 R3) (select state8 R3)) (= rd7 R3))
(or (= (select state7 R4) (select state8 R4)) (= rd7 R4))
(or (= (select state7 R5) (select state8 R5)) (= rd7 R5))
(or (= (select state7 R6) (select state8 R6)) (= rd7 R6))
(or (= (select state7 R7) (select state8 R7)) (= rd7 R7))
(or (= (select state7 R8) (select state8 R8)) (= rd7 R8))
(or (= (select state7 R9) (select state8 R9)) (= rd7 R9))
(or (= (select state7 R10) (select state8 R10)) (= rd7 R10))
(or (= (select state7 R11) (select state8 R11)) (= rd7 R11))
(or (= (select state7 R12) (select state8 R12)) (= rd7 R12))
(or (= (select state7 SP) (select state8 SP)) (= rd7 SP))
(or (= (select state7 LR) (select state8 LR)) (= rd7 LR))
(or
; execute_2
(and
(or (= oper7 MOV) (= oper7 MVN))
(or (not (= oper7 MOV)) (= rd7_val flex_val))
(or (not (= oper7 MVN)) (= rd7_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper7 ADD) (= oper7 SUB) (= oper7 AND)) 
(or (not (= oper7 ADD)) (= rd7_val (bvadd rn7_val flex_val)))
(or (not (= oper7 SUB)) (= rd7_val (bvsub rn7_val flex_val)))
(or (not (= oper7 AND)) (= rd7_val (bvand rn7_val flex_val)))
)
)
; set flag7s or not
(or 
(and (= flag7 N) (= (select state7 CPSR) (select state8 CPSR))) 
(and (= flag7 S) 
(ite 
(= (select state8 rd7) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state8 CPSR)) #b1) ;Z flag7 is 1
(= ((_ extract 30 30) (select state8 CPSR)) #b0) ;Z flag7 is 0
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
(assert (=> (transition state8 state9)
(and
; pc incremented
(= (select state9 PC) (bvadd (select state8 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond8ition is false
(not
(or
(and (= cond8 EQ) (= ((_ extract 30 30) (select state8 CPSR)) #b1))
(and (= cond8 NE) (= ((_ extract 30 30) (select state8 CPSR)) #b0))
(and (= cond8 CS) (= ((_ extract 29 29) (select state8 CPSR)) #b1))
(and (= cond8 CC) (= ((_ extract 29 29) (select state8 CPSR)) #b0))
(and (= cond8 MI) (= ((_ extract 31 31) (select state8 CPSR)) #b1))
(and (= cond8 PL) (= ((_ extract 31 31) (select state8 CPSR)) #b0))
(and (= cond8 VS) (= ((_ extract 28 28) (select state8 CPSR)) #b1))
(and (= cond8 VC) (= ((_ extract 28 28) (select state8 CPSR)) #b0))
(and (= cond8 HI) (and (= ((_ extract 29 29) (select state8 CPSR)) #b1) (= ((_ extract 30 30) (select state8 CPSR)) #b0) ))
(and (= cond8 LS) (or (= ((_ extract 29 29) (select state8 CPSR)) #b0) (= ((_ extract 30 30) (select state8 CPSR)) #b1) ))
(and (= cond8 GE) (and (= ((_ extract 31 31) (select state8 CPSR)) #b1) (= ((_ extract 28 28) (select state8 CPSR)) #b1) ) )
(and (= cond8 LT) (or (= ((_ extract 31 31) (select state8 CPSR)) #b0) (= ((_ extract 28 28) (select state8 CPSR)) #b0) ) )
(and 
(= cond8 GT)
(and 
(= ((_ extract 30 30) (select state8 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state8 CPSR)) #b1) (= ((_ extract 28 28) (select state8 CPSR)) #b1)) 
)
)
(and 
(= cond8 LE)
(or 
(= ((_ extract 30 30) (select state8 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state8 CPSR)) #b1) (= ((_ extract 28 28) (select state8 CPSR)) #b1))) )
)
(= cond8 AL)
)
) 
; R0 to CPSR are equal
(= (select state8 R0) (select state9 R0)) 
(= (select state8 R1) (select state9 R1))
(= (select state8 R2) (select state9 R2))
(= (select state8 R3) (select state9 R3))
(= (select state8 R4) (select state9 R4))
(= (select state8 R5) (select state9 R5))
(= (select state8 R6) (select state9 R6))
(= (select state8 R7) (select state9 R7))
(= (select state8 R8) (select state9 R8))
(= (select state8 R9) (select state9 R9))
(= (select state8 R10) (select state9 R10))
(= (select state8 R11) (select state9 R11))
(= (select state8 R12) (select state9 R12))
(= (select state8 SP) (select state9 SP))
(= (select state8 LR) (select state9 LR)) 
(= (select state8 CPSR) (select state9 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm8_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm8))) (select state8 ro8))))
(ite
(= barrel_op8 LSL)
(bvshl val_to_shift barrel_num8)
(ite 
(= barrel_op8 LSR)
(bvlshr val_to_shift barrel_num8)
(bvashr val_to_shift barrel_num8)
)
)
)
)
(rd8_val (select state9 rd8)) (rn8_val (select state8 rn8))
)
; instruction is executed
(and
; cond8ition is true
(or
(and (= cond8 EQ) (= ((_ extract 30 30) (select state8 CPSR)) #b1))
(and (= cond8 NE) (= ((_ extract 30 30) (select state8 CPSR)) #b0))
(and (= cond8 CS) (= ((_ extract 29 29) (select state8 CPSR)) #b1))
(and (= cond8 CC) (= ((_ extract 29 29) (select state8 CPSR)) #b0))
(and (= cond8 MI) (= ((_ extract 31 31) (select state8 CPSR)) #b1))
(and (= cond8 PL) (= ((_ extract 31 31) (select state8 CPSR)) #b0))
(and (= cond8 VS) (= ((_ extract 28 28) (select state8 CPSR)) #b1))
(and (= cond8 VC) (= ((_ extract 28 28) (select state8 CPSR)) #b0))
(and (= cond8 HI) (and (= ((_ extract 29 29) (select state8 CPSR)) #b1) (= ((_ extract 30 30) (select state8 CPSR)) #b0) ))
(and (= cond8 LS) (or (= ((_ extract 29 29) (select state8 CPSR)) #b0) (= ((_ extract 30 30) (select state8 CPSR)) #b1) ))
(and (= cond8 GE) (and (= ((_ extract 31 31) (select state8 CPSR)) #b1) (= ((_ extract 28 28) (select state8 CPSR)) #b1) ) )
(and (= cond8 LT) (or (= ((_ extract 31 31) (select state8 CPSR)) #b0) (= ((_ extract 28 28) (select state8 CPSR)) #b0) ) )
(and 
(= cond8 GT) 
(and 
(= ((_ extract 30 30) (select state8 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state8 CPSR)) #b1) (= ((_ extract 28 28) (select state8 CPSR)) #b1)) 
)
)
(and 
(= cond8 LE)
(or 
(= ((_ extract 30 30) (select state8 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state8 CPSR)) #b1) (= ((_ extract 28 28) (select state8 CPSR)) #b1))) )
)
(= cond8 AL)
)
(or
; execute_compare
(and
(or (= oper8 CMP)) 
; R0 to LR are equal
(= (select state8 R0) (select state9 R0)) 
(= (select state8 R1) (select state9 R1))
(= (select state8 R2) (select state9 R2))
(= (select state8 R3) (select state9 R3))
(= (select state8 R4) (select state9 R4))
(= (select state8 R5) (select state9 R5))
(= (select state8 R6) (select state9 R6))
(= (select state8 R7) (select state9 R7))
(= (select state8 R8) (select state9 R8))
(= (select state8 R9) (select state9 R9))
(= (select state8 R10) (select state9 R10))
(= (select state8 R11) (select state9 R11))
(= (select state8 R12) (select state9 R12))
(= (select state8 SP) (select state9 SP))
(= (select state8 LR) (select state9 LR))
(or (not (= oper8 CMP)) 
(ite (= rn8_val flex_val)
(= ((_ extract 30 30) (select state9 CPSR)) #b1) ; Z flag8 is 1 
(= ((_ extract 30 30) (select state9 CPSR)) #b0) ; Z flag8 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state8 R0) (select state9 R0)) (= rd8 R0))  
(or (= (select state8 R1) (select state9 R1)) (= rd8 R1))
(or (= (select state8 R2) (select state9 R2)) (= rd8 R2))
(or (= (select state8 R3) (select state9 R3)) (= rd8 R3))
(or (= (select state8 R4) (select state9 R4)) (= rd8 R4))
(or (= (select state8 R5) (select state9 R5)) (= rd8 R5))
(or (= (select state8 R6) (select state9 R6)) (= rd8 R6))
(or (= (select state8 R7) (select state9 R7)) (= rd8 R7))
(or (= (select state8 R8) (select state9 R8)) (= rd8 R8))
(or (= (select state8 R9) (select state9 R9)) (= rd8 R9))
(or (= (select state8 R10) (select state9 R10)) (= rd8 R10))
(or (= (select state8 R11) (select state9 R11)) (= rd8 R11))
(or (= (select state8 R12) (select state9 R12)) (= rd8 R12))
(or (= (select state8 SP) (select state9 SP)) (= rd8 SP))
(or (= (select state8 LR) (select state9 LR)) (= rd8 LR))
(or
; execute_2
(and
(or (= oper8 MOV) (= oper8 MVN))
(or (not (= oper8 MOV)) (= rd8_val flex_val))
(or (not (= oper8 MVN)) (= rd8_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper8 ADD) (= oper8 SUB) (= oper8 AND)) 
(or (not (= oper8 ADD)) (= rd8_val (bvadd rn8_val flex_val)))
(or (not (= oper8 SUB)) (= rd8_val (bvsub rn8_val flex_val)))
(or (not (= oper8 AND)) (= rd8_val (bvand rn8_val flex_val)))
)
)
; set flag8s or not
(or 
(and (= flag8 N) (= (select state8 CPSR) (select state9 CPSR))) 
(and (= flag8 S) 
(ite 
(= (select state9 rd8) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state9 CPSR)) #b1) ;Z flag8 is 1
(= ((_ extract 30 30) (select state9 CPSR)) #b0) ;Z flag8 is 0
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
(assert (=> (transition state9 state10)
(and
; pc incremented
(= (select state10 PC) (bvadd (select state9 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond9ition is false
(not
(or
(and (= cond9 EQ) (= ((_ extract 30 30) (select state9 CPSR)) #b1))
(and (= cond9 NE) (= ((_ extract 30 30) (select state9 CPSR)) #b0))
(and (= cond9 CS) (= ((_ extract 29 29) (select state9 CPSR)) #b1))
(and (= cond9 CC) (= ((_ extract 29 29) (select state9 CPSR)) #b0))
(and (= cond9 MI) (= ((_ extract 31 31) (select state9 CPSR)) #b1))
(and (= cond9 PL) (= ((_ extract 31 31) (select state9 CPSR)) #b0))
(and (= cond9 VS) (= ((_ extract 28 28) (select state9 CPSR)) #b1))
(and (= cond9 VC) (= ((_ extract 28 28) (select state9 CPSR)) #b0))
(and (= cond9 HI) (and (= ((_ extract 29 29) (select state9 CPSR)) #b1) (= ((_ extract 30 30) (select state9 CPSR)) #b0) ))
(and (= cond9 LS) (or (= ((_ extract 29 29) (select state9 CPSR)) #b0) (= ((_ extract 30 30) (select state9 CPSR)) #b1) ))
(and (= cond9 GE) (and (= ((_ extract 31 31) (select state9 CPSR)) #b1) (= ((_ extract 28 28) (select state9 CPSR)) #b1) ) )
(and (= cond9 LT) (or (= ((_ extract 31 31) (select state9 CPSR)) #b0) (= ((_ extract 28 28) (select state9 CPSR)) #b0) ) )
(and 
(= cond9 GT)
(and 
(= ((_ extract 30 30) (select state9 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state9 CPSR)) #b1) (= ((_ extract 28 28) (select state9 CPSR)) #b1)) 
)
)
(and 
(= cond9 LE)
(or 
(= ((_ extract 30 30) (select state9 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state9 CPSR)) #b1) (= ((_ extract 28 28) (select state9 CPSR)) #b1))) )
)
(= cond9 AL)
)
) 
; R0 to CPSR are equal
(= (select state9 R0) (select state10 R0)) 
(= (select state9 R1) (select state10 R1))
(= (select state9 R2) (select state10 R2))
(= (select state9 R3) (select state10 R3))
(= (select state9 R4) (select state10 R4))
(= (select state9 R5) (select state10 R5))
(= (select state9 R6) (select state10 R6))
(= (select state9 R7) (select state10 R7))
(= (select state9 R8) (select state10 R8))
(= (select state9 R9) (select state10 R9))
(= (select state9 R10) (select state10 R10))
(= (select state9 R11) (select state10 R11))
(= (select state9 R12) (select state10 R12))
(= (select state9 SP) (select state10 SP))
(= (select state9 LR) (select state10 LR)) 
(= (select state9 CPSR) (select state10 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm9_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm9))) (select state9 ro9))))
(ite
(= barrel_op9 LSL)
(bvshl val_to_shift barrel_num9)
(ite 
(= barrel_op9 LSR)
(bvlshr val_to_shift barrel_num9)
(bvashr val_to_shift barrel_num9)
)
)
)
)
(rd9_val (select state10 rd9)) (rn9_val (select state9 rn9))
)
; instruction is executed
(and
; cond9ition is true
(or
(and (= cond9 EQ) (= ((_ extract 30 30) (select state9 CPSR)) #b1))
(and (= cond9 NE) (= ((_ extract 30 30) (select state9 CPSR)) #b0))
(and (= cond9 CS) (= ((_ extract 29 29) (select state9 CPSR)) #b1))
(and (= cond9 CC) (= ((_ extract 29 29) (select state9 CPSR)) #b0))
(and (= cond9 MI) (= ((_ extract 31 31) (select state9 CPSR)) #b1))
(and (= cond9 PL) (= ((_ extract 31 31) (select state9 CPSR)) #b0))
(and (= cond9 VS) (= ((_ extract 28 28) (select state9 CPSR)) #b1))
(and (= cond9 VC) (= ((_ extract 28 28) (select state9 CPSR)) #b0))
(and (= cond9 HI) (and (= ((_ extract 29 29) (select state9 CPSR)) #b1) (= ((_ extract 30 30) (select state9 CPSR)) #b0) ))
(and (= cond9 LS) (or (= ((_ extract 29 29) (select state9 CPSR)) #b0) (= ((_ extract 30 30) (select state9 CPSR)) #b1) ))
(and (= cond9 GE) (and (= ((_ extract 31 31) (select state9 CPSR)) #b1) (= ((_ extract 28 28) (select state9 CPSR)) #b1) ) )
(and (= cond9 LT) (or (= ((_ extract 31 31) (select state9 CPSR)) #b0) (= ((_ extract 28 28) (select state9 CPSR)) #b0) ) )
(and 
(= cond9 GT) 
(and 
(= ((_ extract 30 30) (select state9 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state9 CPSR)) #b1) (= ((_ extract 28 28) (select state9 CPSR)) #b1)) 
)
)
(and 
(= cond9 LE)
(or 
(= ((_ extract 30 30) (select state9 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state9 CPSR)) #b1) (= ((_ extract 28 28) (select state9 CPSR)) #b1))) )
)
(= cond9 AL)
)
(or
; execute_compare
(and
(or (= oper9 CMP)) 
; R0 to LR are equal
(= (select state9 R0) (select state10 R0)) 
(= (select state9 R1) (select state10 R1))
(= (select state9 R2) (select state10 R2))
(= (select state9 R3) (select state10 R3))
(= (select state9 R4) (select state10 R4))
(= (select state9 R5) (select state10 R5))
(= (select state9 R6) (select state10 R6))
(= (select state9 R7) (select state10 R7))
(= (select state9 R8) (select state10 R8))
(= (select state9 R9) (select state10 R9))
(= (select state9 R10) (select state10 R10))
(= (select state9 R11) (select state10 R11))
(= (select state9 R12) (select state10 R12))
(= (select state9 SP) (select state10 SP))
(= (select state9 LR) (select state10 LR))
(or (not (= oper9 CMP)) 
(ite (= rn9_val flex_val)
(= ((_ extract 30 30) (select state10 CPSR)) #b1) ; Z flag9 is 1 
(= ((_ extract 30 30) (select state10 CPSR)) #b0) ; Z flag9 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state9 R0) (select state10 R0)) (= rd9 R0))  
(or (= (select state9 R1) (select state10 R1)) (= rd9 R1))
(or (= (select state9 R2) (select state10 R2)) (= rd9 R2))
(or (= (select state9 R3) (select state10 R3)) (= rd9 R3))
(or (= (select state9 R4) (select state10 R4)) (= rd9 R4))
(or (= (select state9 R5) (select state10 R5)) (= rd9 R5))
(or (= (select state9 R6) (select state10 R6)) (= rd9 R6))
(or (= (select state9 R7) (select state10 R7)) (= rd9 R7))
(or (= (select state9 R8) (select state10 R8)) (= rd9 R8))
(or (= (select state9 R9) (select state10 R9)) (= rd9 R9))
(or (= (select state9 R10) (select state10 R10)) (= rd9 R10))
(or (= (select state9 R11) (select state10 R11)) (= rd9 R11))
(or (= (select state9 R12) (select state10 R12)) (= rd9 R12))
(or (= (select state9 SP) (select state10 SP)) (= rd9 SP))
(or (= (select state9 LR) (select state10 LR)) (= rd9 LR))
(or
; execute_2
(and
(or (= oper9 MOV) (= oper9 MVN))
(or (not (= oper9 MOV)) (= rd9_val flex_val))
(or (not (= oper9 MVN)) (= rd9_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper9 ADD) (= oper9 SUB) (= oper9 AND)) 
(or (not (= oper9 ADD)) (= rd9_val (bvadd rn9_val flex_val)))
(or (not (= oper9 SUB)) (= rd9_val (bvsub rn9_val flex_val)))
(or (not (= oper9 AND)) (= rd9_val (bvand rn9_val flex_val)))
)
)
; set flag9s or not
(or 
(and (= flag9 N) (= (select state9 CPSR) (select state10 CPSR))) 
(and (= flag9 S) 
(ite 
(= (select state10 rd9) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state10 CPSR)) #b1) ;Z flag9 is 1
(= ((_ extract 30 30) (select state10 CPSR)) #b0) ;Z flag9 is 0
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
(assert (=> (transition state10 state11)
(and
; pc incremented
(= (select state11 PC) (bvadd (select state10 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond10ition is false
(not
(or
(and (= cond10 EQ) (= ((_ extract 30 30) (select state10 CPSR)) #b1))
(and (= cond10 NE) (= ((_ extract 30 30) (select state10 CPSR)) #b0))
(and (= cond10 CS) (= ((_ extract 29 29) (select state10 CPSR)) #b1))
(and (= cond10 CC) (= ((_ extract 29 29) (select state10 CPSR)) #b0))
(and (= cond10 MI) (= ((_ extract 31 31) (select state10 CPSR)) #b1))
(and (= cond10 PL) (= ((_ extract 31 31) (select state10 CPSR)) #b0))
(and (= cond10 VS) (= ((_ extract 28 28) (select state10 CPSR)) #b1))
(and (= cond10 VC) (= ((_ extract 28 28) (select state10 CPSR)) #b0))
(and (= cond10 HI) (and (= ((_ extract 29 29) (select state10 CPSR)) #b1) (= ((_ extract 30 30) (select state10 CPSR)) #b0) ))
(and (= cond10 LS) (or (= ((_ extract 29 29) (select state10 CPSR)) #b0) (= ((_ extract 30 30) (select state10 CPSR)) #b1) ))
(and (= cond10 GE) (and (= ((_ extract 31 31) (select state10 CPSR)) #b1) (= ((_ extract 28 28) (select state10 CPSR)) #b1) ) )
(and (= cond10 LT) (or (= ((_ extract 31 31) (select state10 CPSR)) #b0) (= ((_ extract 28 28) (select state10 CPSR)) #b0) ) )
(and 
(= cond10 GT)
(and 
(= ((_ extract 30 30) (select state10 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state10 CPSR)) #b1) (= ((_ extract 28 28) (select state10 CPSR)) #b1)) 
)
)
(and 
(= cond10 LE)
(or 
(= ((_ extract 30 30) (select state10 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state10 CPSR)) #b1) (= ((_ extract 28 28) (select state10 CPSR)) #b1))) )
)
(= cond10 AL)
)
) 
; R0 to CPSR are equal
(= (select state10 R0) (select state11 R0)) 
(= (select state10 R1) (select state11 R1))
(= (select state10 R2) (select state11 R2))
(= (select state10 R3) (select state11 R3))
(= (select state10 R4) (select state11 R4))
(= (select state10 R5) (select state11 R5))
(= (select state10 R6) (select state11 R6))
(= (select state10 R7) (select state11 R7))
(= (select state10 R8) (select state11 R8))
(= (select state10 R9) (select state11 R9))
(= (select state10 R10) (select state11 R10))
(= (select state10 R11) (select state11 R11))
(= (select state10 R12) (select state11 R12))
(= (select state10 SP) (select state11 SP))
(= (select state10 LR) (select state11 LR)) 
(= (select state10 CPSR) (select state11 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm10_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm10))) (select state10 ro10))))
(ite
(= barrel_op10 LSL)
(bvshl val_to_shift barrel_num10)
(ite 
(= barrel_op10 LSR)
(bvlshr val_to_shift barrel_num10)
(bvashr val_to_shift barrel_num10)
)
)
)
)
(rd10_val (select state11 rd10)) (rn10_val (select state10 rn10))
)
; instruction is executed
(and
; cond10ition is true
(or
(and (= cond10 EQ) (= ((_ extract 30 30) (select state10 CPSR)) #b1))
(and (= cond10 NE) (= ((_ extract 30 30) (select state10 CPSR)) #b0))
(and (= cond10 CS) (= ((_ extract 29 29) (select state10 CPSR)) #b1))
(and (= cond10 CC) (= ((_ extract 29 29) (select state10 CPSR)) #b0))
(and (= cond10 MI) (= ((_ extract 31 31) (select state10 CPSR)) #b1))
(and (= cond10 PL) (= ((_ extract 31 31) (select state10 CPSR)) #b0))
(and (= cond10 VS) (= ((_ extract 28 28) (select state10 CPSR)) #b1))
(and (= cond10 VC) (= ((_ extract 28 28) (select state10 CPSR)) #b0))
(and (= cond10 HI) (and (= ((_ extract 29 29) (select state10 CPSR)) #b1) (= ((_ extract 30 30) (select state10 CPSR)) #b0) ))
(and (= cond10 LS) (or (= ((_ extract 29 29) (select state10 CPSR)) #b0) (= ((_ extract 30 30) (select state10 CPSR)) #b1) ))
(and (= cond10 GE) (and (= ((_ extract 31 31) (select state10 CPSR)) #b1) (= ((_ extract 28 28) (select state10 CPSR)) #b1) ) )
(and (= cond10 LT) (or (= ((_ extract 31 31) (select state10 CPSR)) #b0) (= ((_ extract 28 28) (select state10 CPSR)) #b0) ) )
(and 
(= cond10 GT) 
(and 
(= ((_ extract 30 30) (select state10 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state10 CPSR)) #b1) (= ((_ extract 28 28) (select state10 CPSR)) #b1)) 
)
)
(and 
(= cond10 LE)
(or 
(= ((_ extract 30 30) (select state10 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state10 CPSR)) #b1) (= ((_ extract 28 28) (select state10 CPSR)) #b1))) )
)
(= cond10 AL)
)
(or
; execute_compare
(and
(or (= oper10 CMP)) 
; R0 to LR are equal
(= (select state10 R0) (select state11 R0)) 
(= (select state10 R1) (select state11 R1))
(= (select state10 R2) (select state11 R2))
(= (select state10 R3) (select state11 R3))
(= (select state10 R4) (select state11 R4))
(= (select state10 R5) (select state11 R5))
(= (select state10 R6) (select state11 R6))
(= (select state10 R7) (select state11 R7))
(= (select state10 R8) (select state11 R8))
(= (select state10 R9) (select state11 R9))
(= (select state10 R10) (select state11 R10))
(= (select state10 R11) (select state11 R11))
(= (select state10 R12) (select state11 R12))
(= (select state10 SP) (select state11 SP))
(= (select state10 LR) (select state11 LR))
(or (not (= oper10 CMP)) 
(ite (= rn10_val flex_val)
(= ((_ extract 30 30) (select state11 CPSR)) #b1) ; Z flag10 is 1 
(= ((_ extract 30 30) (select state11 CPSR)) #b0) ; Z flag10 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state10 R0) (select state11 R0)) (= rd10 R0))  
(or (= (select state10 R1) (select state11 R1)) (= rd10 R1))
(or (= (select state10 R2) (select state11 R2)) (= rd10 R2))
(or (= (select state10 R3) (select state11 R3)) (= rd10 R3))
(or (= (select state10 R4) (select state11 R4)) (= rd10 R4))
(or (= (select state10 R5) (select state11 R5)) (= rd10 R5))
(or (= (select state10 R6) (select state11 R6)) (= rd10 R6))
(or (= (select state10 R7) (select state11 R7)) (= rd10 R7))
(or (= (select state10 R8) (select state11 R8)) (= rd10 R8))
(or (= (select state10 R9) (select state11 R9)) (= rd10 R9))
(or (= (select state10 R10) (select state11 R10)) (= rd10 R10))
(or (= (select state10 R11) (select state11 R11)) (= rd10 R11))
(or (= (select state10 R12) (select state11 R12)) (= rd10 R12))
(or (= (select state10 SP) (select state11 SP)) (= rd10 SP))
(or (= (select state10 LR) (select state11 LR)) (= rd10 LR))
(or
; execute_2
(and
(or (= oper10 MOV) (= oper10 MVN))
(or (not (= oper10 MOV)) (= rd10_val flex_val))
(or (not (= oper10 MVN)) (= rd10_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper10 ADD) (= oper10 SUB) (= oper10 AND)) 
(or (not (= oper10 ADD)) (= rd10_val (bvadd rn10_val flex_val)))
(or (not (= oper10 SUB)) (= rd10_val (bvsub rn10_val flex_val)))
(or (not (= oper10 AND)) (= rd10_val (bvand rn10_val flex_val)))
)
)
; set flag10s or not
(or 
(and (= flag10 N) (= (select state10 CPSR) (select state11 CPSR))) 
(and (= flag10 S) 
(ite 
(= (select state11 rd10) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state11 CPSR)) #b1) ;Z flag10 is 1
(= ((_ extract 30 30) (select state11 CPSR)) #b0) ;Z flag10 is 0
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
(assert (=> (transition state11 state12)
(and
; pc incremented
(= (select state12 PC) (bvadd (select state11 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond11ition is false
(not
(or
(and (= cond11 EQ) (= ((_ extract 30 30) (select state11 CPSR)) #b1))
(and (= cond11 NE) (= ((_ extract 30 30) (select state11 CPSR)) #b0))
(and (= cond11 CS) (= ((_ extract 29 29) (select state11 CPSR)) #b1))
(and (= cond11 CC) (= ((_ extract 29 29) (select state11 CPSR)) #b0))
(and (= cond11 MI) (= ((_ extract 31 31) (select state11 CPSR)) #b1))
(and (= cond11 PL) (= ((_ extract 31 31) (select state11 CPSR)) #b0))
(and (= cond11 VS) (= ((_ extract 28 28) (select state11 CPSR)) #b1))
(and (= cond11 VC) (= ((_ extract 28 28) (select state11 CPSR)) #b0))
(and (= cond11 HI) (and (= ((_ extract 29 29) (select state11 CPSR)) #b1) (= ((_ extract 30 30) (select state11 CPSR)) #b0) ))
(and (= cond11 LS) (or (= ((_ extract 29 29) (select state11 CPSR)) #b0) (= ((_ extract 30 30) (select state11 CPSR)) #b1) ))
(and (= cond11 GE) (and (= ((_ extract 31 31) (select state11 CPSR)) #b1) (= ((_ extract 28 28) (select state11 CPSR)) #b1) ) )
(and (= cond11 LT) (or (= ((_ extract 31 31) (select state11 CPSR)) #b0) (= ((_ extract 28 28) (select state11 CPSR)) #b0) ) )
(and 
(= cond11 GT)
(and 
(= ((_ extract 30 30) (select state11 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state11 CPSR)) #b1) (= ((_ extract 28 28) (select state11 CPSR)) #b1)) 
)
)
(and 
(= cond11 LE)
(or 
(= ((_ extract 30 30) (select state11 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state11 CPSR)) #b1) (= ((_ extract 28 28) (select state11 CPSR)) #b1))) )
)
(= cond11 AL)
)
) 
; R0 to CPSR are equal
(= (select state11 R0) (select state12 R0)) 
(= (select state11 R1) (select state12 R1))
(= (select state11 R2) (select state12 R2))
(= (select state11 R3) (select state12 R3))
(= (select state11 R4) (select state12 R4))
(= (select state11 R5) (select state12 R5))
(= (select state11 R6) (select state12 R6))
(= (select state11 R7) (select state12 R7))
(= (select state11 R8) (select state12 R8))
(= (select state11 R9) (select state12 R9))
(= (select state11 R10) (select state12 R10))
(= (select state11 R11) (select state12 R11))
(= (select state11 R12) (select state12 R12))
(= (select state11 SP) (select state12 SP))
(= (select state11 LR) (select state12 LR)) 
(= (select state11 CPSR) (select state12 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm11_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm11))) (select state11 ro11))))
(ite
(= barrel_op11 LSL)
(bvshl val_to_shift barrel_num11)
(ite 
(= barrel_op11 LSR)
(bvlshr val_to_shift barrel_num11)
(bvashr val_to_shift barrel_num11)
)
)
)
)
(rd11_val (select state12 rd11)) (rn11_val (select state11 rn11))
)
; instruction is executed
(and
; cond11ition is true
(or
(and (= cond11 EQ) (= ((_ extract 30 30) (select state11 CPSR)) #b1))
(and (= cond11 NE) (= ((_ extract 30 30) (select state11 CPSR)) #b0))
(and (= cond11 CS) (= ((_ extract 29 29) (select state11 CPSR)) #b1))
(and (= cond11 CC) (= ((_ extract 29 29) (select state11 CPSR)) #b0))
(and (= cond11 MI) (= ((_ extract 31 31) (select state11 CPSR)) #b1))
(and (= cond11 PL) (= ((_ extract 31 31) (select state11 CPSR)) #b0))
(and (= cond11 VS) (= ((_ extract 28 28) (select state11 CPSR)) #b1))
(and (= cond11 VC) (= ((_ extract 28 28) (select state11 CPSR)) #b0))
(and (= cond11 HI) (and (= ((_ extract 29 29) (select state11 CPSR)) #b1) (= ((_ extract 30 30) (select state11 CPSR)) #b0) ))
(and (= cond11 LS) (or (= ((_ extract 29 29) (select state11 CPSR)) #b0) (= ((_ extract 30 30) (select state11 CPSR)) #b1) ))
(and (= cond11 GE) (and (= ((_ extract 31 31) (select state11 CPSR)) #b1) (= ((_ extract 28 28) (select state11 CPSR)) #b1) ) )
(and (= cond11 LT) (or (= ((_ extract 31 31) (select state11 CPSR)) #b0) (= ((_ extract 28 28) (select state11 CPSR)) #b0) ) )
(and 
(= cond11 GT) 
(and 
(= ((_ extract 30 30) (select state11 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state11 CPSR)) #b1) (= ((_ extract 28 28) (select state11 CPSR)) #b1)) 
)
)
(and 
(= cond11 LE)
(or 
(= ((_ extract 30 30) (select state11 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state11 CPSR)) #b1) (= ((_ extract 28 28) (select state11 CPSR)) #b1))) )
)
(= cond11 AL)
)
(or
; execute_compare
(and
(or (= oper11 CMP)) 
; R0 to LR are equal
(= (select state11 R0) (select state12 R0)) 
(= (select state11 R1) (select state12 R1))
(= (select state11 R2) (select state12 R2))
(= (select state11 R3) (select state12 R3))
(= (select state11 R4) (select state12 R4))
(= (select state11 R5) (select state12 R5))
(= (select state11 R6) (select state12 R6))
(= (select state11 R7) (select state12 R7))
(= (select state11 R8) (select state12 R8))
(= (select state11 R9) (select state12 R9))
(= (select state11 R10) (select state12 R10))
(= (select state11 R11) (select state12 R11))
(= (select state11 R12) (select state12 R12))
(= (select state11 SP) (select state12 SP))
(= (select state11 LR) (select state12 LR))
(or (not (= oper11 CMP)) 
(ite (= rn11_val flex_val)
(= ((_ extract 30 30) (select state12 CPSR)) #b1) ; Z flag11 is 1 
(= ((_ extract 30 30) (select state12 CPSR)) #b0) ; Z flag11 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state11 R0) (select state12 R0)) (= rd11 R0))  
(or (= (select state11 R1) (select state12 R1)) (= rd11 R1))
(or (= (select state11 R2) (select state12 R2)) (= rd11 R2))
(or (= (select state11 R3) (select state12 R3)) (= rd11 R3))
(or (= (select state11 R4) (select state12 R4)) (= rd11 R4))
(or (= (select state11 R5) (select state12 R5)) (= rd11 R5))
(or (= (select state11 R6) (select state12 R6)) (= rd11 R6))
(or (= (select state11 R7) (select state12 R7)) (= rd11 R7))
(or (= (select state11 R8) (select state12 R8)) (= rd11 R8))
(or (= (select state11 R9) (select state12 R9)) (= rd11 R9))
(or (= (select state11 R10) (select state12 R10)) (= rd11 R10))
(or (= (select state11 R11) (select state12 R11)) (= rd11 R11))
(or (= (select state11 R12) (select state12 R12)) (= rd11 R12))
(or (= (select state11 SP) (select state12 SP)) (= rd11 SP))
(or (= (select state11 LR) (select state12 LR)) (= rd11 LR))
(or
; execute_2
(and
(or (= oper11 MOV) (= oper11 MVN))
(or (not (= oper11 MOV)) (= rd11_val flex_val))
(or (not (= oper11 MVN)) (= rd11_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper11 ADD) (= oper11 SUB) (= oper11 AND)) 
(or (not (= oper11 ADD)) (= rd11_val (bvadd rn11_val flex_val)))
(or (not (= oper11 SUB)) (= rd11_val (bvsub rn11_val flex_val)))
(or (not (= oper11 AND)) (= rd11_val (bvand rn11_val flex_val)))
)
)
; set flag11s or not
(or 
(and (= flag11 N) (= (select state11 CPSR) (select state12 CPSR))) 
(and (= flag11 S) 
(ite 
(= (select state12 rd11) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state12 CPSR)) #b1) ;Z flag11 is 1
(= ((_ extract 30 30) (select state12 CPSR)) #b0) ;Z flag11 is 0
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
(assert (=> (transition state12 state13)
(and
; pc incremented
(= (select state13 PC) (bvadd (select state12 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond12ition is false
(not
(or
(and (= cond12 EQ) (= ((_ extract 30 30) (select state12 CPSR)) #b1))
(and (= cond12 NE) (= ((_ extract 30 30) (select state12 CPSR)) #b0))
(and (= cond12 CS) (= ((_ extract 29 29) (select state12 CPSR)) #b1))
(and (= cond12 CC) (= ((_ extract 29 29) (select state12 CPSR)) #b0))
(and (= cond12 MI) (= ((_ extract 31 31) (select state12 CPSR)) #b1))
(and (= cond12 PL) (= ((_ extract 31 31) (select state12 CPSR)) #b0))
(and (= cond12 VS) (= ((_ extract 28 28) (select state12 CPSR)) #b1))
(and (= cond12 VC) (= ((_ extract 28 28) (select state12 CPSR)) #b0))
(and (= cond12 HI) (and (= ((_ extract 29 29) (select state12 CPSR)) #b1) (= ((_ extract 30 30) (select state12 CPSR)) #b0) ))
(and (= cond12 LS) (or (= ((_ extract 29 29) (select state12 CPSR)) #b0) (= ((_ extract 30 30) (select state12 CPSR)) #b1) ))
(and (= cond12 GE) (and (= ((_ extract 31 31) (select state12 CPSR)) #b1) (= ((_ extract 28 28) (select state12 CPSR)) #b1) ) )
(and (= cond12 LT) (or (= ((_ extract 31 31) (select state12 CPSR)) #b0) (= ((_ extract 28 28) (select state12 CPSR)) #b0) ) )
(and 
(= cond12 GT)
(and 
(= ((_ extract 30 30) (select state12 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state12 CPSR)) #b1) (= ((_ extract 28 28) (select state12 CPSR)) #b1)) 
)
)
(and 
(= cond12 LE)
(or 
(= ((_ extract 30 30) (select state12 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state12 CPSR)) #b1) (= ((_ extract 28 28) (select state12 CPSR)) #b1))) )
)
(= cond12 AL)
)
) 
; R0 to CPSR are equal
(= (select state12 R0) (select state13 R0)) 
(= (select state12 R1) (select state13 R1))
(= (select state12 R2) (select state13 R2))
(= (select state12 R3) (select state13 R3))
(= (select state12 R4) (select state13 R4))
(= (select state12 R5) (select state13 R5))
(= (select state12 R6) (select state13 R6))
(= (select state12 R7) (select state13 R7))
(= (select state12 R8) (select state13 R8))
(= (select state12 R9) (select state13 R9))
(= (select state12 R10) (select state13 R10))
(= (select state12 R11) (select state13 R11))
(= (select state12 R12) (select state13 R12))
(= (select state12 SP) (select state13 SP))
(= (select state12 LR) (select state13 LR)) 
(= (select state12 CPSR) (select state13 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm12_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm12))) (select state12 ro12))))
(ite
(= barrel_op12 LSL)
(bvshl val_to_shift barrel_num12)
(ite 
(= barrel_op12 LSR)
(bvlshr val_to_shift barrel_num12)
(bvashr val_to_shift barrel_num12)
)
)
)
)
(rd12_val (select state13 rd12)) (rn12_val (select state12 rn12))
)
; instruction is executed
(and
; cond12ition is true
(or
(and (= cond12 EQ) (= ((_ extract 30 30) (select state12 CPSR)) #b1))
(and (= cond12 NE) (= ((_ extract 30 30) (select state12 CPSR)) #b0))
(and (= cond12 CS) (= ((_ extract 29 29) (select state12 CPSR)) #b1))
(and (= cond12 CC) (= ((_ extract 29 29) (select state12 CPSR)) #b0))
(and (= cond12 MI) (= ((_ extract 31 31) (select state12 CPSR)) #b1))
(and (= cond12 PL) (= ((_ extract 31 31) (select state12 CPSR)) #b0))
(and (= cond12 VS) (= ((_ extract 28 28) (select state12 CPSR)) #b1))
(and (= cond12 VC) (= ((_ extract 28 28) (select state12 CPSR)) #b0))
(and (= cond12 HI) (and (= ((_ extract 29 29) (select state12 CPSR)) #b1) (= ((_ extract 30 30) (select state12 CPSR)) #b0) ))
(and (= cond12 LS) (or (= ((_ extract 29 29) (select state12 CPSR)) #b0) (= ((_ extract 30 30) (select state12 CPSR)) #b1) ))
(and (= cond12 GE) (and (= ((_ extract 31 31) (select state12 CPSR)) #b1) (= ((_ extract 28 28) (select state12 CPSR)) #b1) ) )
(and (= cond12 LT) (or (= ((_ extract 31 31) (select state12 CPSR)) #b0) (= ((_ extract 28 28) (select state12 CPSR)) #b0) ) )
(and 
(= cond12 GT) 
(and 
(= ((_ extract 30 30) (select state12 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state12 CPSR)) #b1) (= ((_ extract 28 28) (select state12 CPSR)) #b1)) 
)
)
(and 
(= cond12 LE)
(or 
(= ((_ extract 30 30) (select state12 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state12 CPSR)) #b1) (= ((_ extract 28 28) (select state12 CPSR)) #b1))) )
)
(= cond12 AL)
)
(or
; execute_compare
(and
(or (= oper12 CMP)) 
; R0 to LR are equal
(= (select state12 R0) (select state13 R0)) 
(= (select state12 R1) (select state13 R1))
(= (select state12 R2) (select state13 R2))
(= (select state12 R3) (select state13 R3))
(= (select state12 R4) (select state13 R4))
(= (select state12 R5) (select state13 R5))
(= (select state12 R6) (select state13 R6))
(= (select state12 R7) (select state13 R7))
(= (select state12 R8) (select state13 R8))
(= (select state12 R9) (select state13 R9))
(= (select state12 R10) (select state13 R10))
(= (select state12 R11) (select state13 R11))
(= (select state12 R12) (select state13 R12))
(= (select state12 SP) (select state13 SP))
(= (select state12 LR) (select state13 LR))
(or (not (= oper12 CMP)) 
(ite (= rn12_val flex_val)
(= ((_ extract 30 30) (select state13 CPSR)) #b1) ; Z flag12 is 1 
(= ((_ extract 30 30) (select state13 CPSR)) #b0) ; Z flag12 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state12 R0) (select state13 R0)) (= rd12 R0))  
(or (= (select state12 R1) (select state13 R1)) (= rd12 R1))
(or (= (select state12 R2) (select state13 R2)) (= rd12 R2))
(or (= (select state12 R3) (select state13 R3)) (= rd12 R3))
(or (= (select state12 R4) (select state13 R4)) (= rd12 R4))
(or (= (select state12 R5) (select state13 R5)) (= rd12 R5))
(or (= (select state12 R6) (select state13 R6)) (= rd12 R6))
(or (= (select state12 R7) (select state13 R7)) (= rd12 R7))
(or (= (select state12 R8) (select state13 R8)) (= rd12 R8))
(or (= (select state12 R9) (select state13 R9)) (= rd12 R9))
(or (= (select state12 R10) (select state13 R10)) (= rd12 R10))
(or (= (select state12 R11) (select state13 R11)) (= rd12 R11))
(or (= (select state12 R12) (select state13 R12)) (= rd12 R12))
(or (= (select state12 SP) (select state13 SP)) (= rd12 SP))
(or (= (select state12 LR) (select state13 LR)) (= rd12 LR))
(or
; execute_2
(and
(or (= oper12 MOV) (= oper12 MVN))
(or (not (= oper12 MOV)) (= rd12_val flex_val))
(or (not (= oper12 MVN)) (= rd12_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper12 ADD) (= oper12 SUB) (= oper12 AND)) 
(or (not (= oper12 ADD)) (= rd12_val (bvadd rn12_val flex_val)))
(or (not (= oper12 SUB)) (= rd12_val (bvsub rn12_val flex_val)))
(or (not (= oper12 AND)) (= rd12_val (bvand rn12_val flex_val)))
)
)
; set flag12s or not
(or 
(and (= flag12 N) (= (select state12 CPSR) (select state13 CPSR))) 
(and (= flag12 S) 
(ite 
(= (select state13 rd12) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state13 CPSR)) #b1) ;Z flag12 is 1
(= ((_ extract 30 30) (select state13 CPSR)) #b0) ;Z flag12 is 0
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
(assert (=> (transition state13 state14)
(and
; pc incremented
(= (select state14 PC) (bvadd (select state13 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond13ition is false
(not
(or
(and (= cond13 EQ) (= ((_ extract 30 30) (select state13 CPSR)) #b1))
(and (= cond13 NE) (= ((_ extract 30 30) (select state13 CPSR)) #b0))
(and (= cond13 CS) (= ((_ extract 29 29) (select state13 CPSR)) #b1))
(and (= cond13 CC) (= ((_ extract 29 29) (select state13 CPSR)) #b0))
(and (= cond13 MI) (= ((_ extract 31 31) (select state13 CPSR)) #b1))
(and (= cond13 PL) (= ((_ extract 31 31) (select state13 CPSR)) #b0))
(and (= cond13 VS) (= ((_ extract 28 28) (select state13 CPSR)) #b1))
(and (= cond13 VC) (= ((_ extract 28 28) (select state13 CPSR)) #b0))
(and (= cond13 HI) (and (= ((_ extract 29 29) (select state13 CPSR)) #b1) (= ((_ extract 30 30) (select state13 CPSR)) #b0) ))
(and (= cond13 LS) (or (= ((_ extract 29 29) (select state13 CPSR)) #b0) (= ((_ extract 30 30) (select state13 CPSR)) #b1) ))
(and (= cond13 GE) (and (= ((_ extract 31 31) (select state13 CPSR)) #b1) (= ((_ extract 28 28) (select state13 CPSR)) #b1) ) )
(and (= cond13 LT) (or (= ((_ extract 31 31) (select state13 CPSR)) #b0) (= ((_ extract 28 28) (select state13 CPSR)) #b0) ) )
(and 
(= cond13 GT)
(and 
(= ((_ extract 30 30) (select state13 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state13 CPSR)) #b1) (= ((_ extract 28 28) (select state13 CPSR)) #b1)) 
)
)
(and 
(= cond13 LE)
(or 
(= ((_ extract 30 30) (select state13 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state13 CPSR)) #b1) (= ((_ extract 28 28) (select state13 CPSR)) #b1))) )
)
(= cond13 AL)
)
) 
; R0 to CPSR are equal
(= (select state13 R0) (select state14 R0)) 
(= (select state13 R1) (select state14 R1))
(= (select state13 R2) (select state14 R2))
(= (select state13 R3) (select state14 R3))
(= (select state13 R4) (select state14 R4))
(= (select state13 R5) (select state14 R5))
(= (select state13 R6) (select state14 R6))
(= (select state13 R7) (select state14 R7))
(= (select state13 R8) (select state14 R8))
(= (select state13 R9) (select state14 R9))
(= (select state13 R10) (select state14 R10))
(= (select state13 R11) (select state14 R11))
(= (select state13 R12) (select state14 R12))
(= (select state13 SP) (select state14 SP))
(= (select state13 LR) (select state14 LR)) 
(= (select state13 CPSR) (select state14 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm13_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm13))) (select state13 ro13))))
(ite
(= barrel_op13 LSL)
(bvshl val_to_shift barrel_num13)
(ite 
(= barrel_op13 LSR)
(bvlshr val_to_shift barrel_num13)
(bvashr val_to_shift barrel_num13)
)
)
)
)
(rd13_val (select state14 rd13)) (rn13_val (select state13 rn13))
)
; instruction is executed
(and
; cond13ition is true
(or
(and (= cond13 EQ) (= ((_ extract 30 30) (select state13 CPSR)) #b1))
(and (= cond13 NE) (= ((_ extract 30 30) (select state13 CPSR)) #b0))
(and (= cond13 CS) (= ((_ extract 29 29) (select state13 CPSR)) #b1))
(and (= cond13 CC) (= ((_ extract 29 29) (select state13 CPSR)) #b0))
(and (= cond13 MI) (= ((_ extract 31 31) (select state13 CPSR)) #b1))
(and (= cond13 PL) (= ((_ extract 31 31) (select state13 CPSR)) #b0))
(and (= cond13 VS) (= ((_ extract 28 28) (select state13 CPSR)) #b1))
(and (= cond13 VC) (= ((_ extract 28 28) (select state13 CPSR)) #b0))
(and (= cond13 HI) (and (= ((_ extract 29 29) (select state13 CPSR)) #b1) (= ((_ extract 30 30) (select state13 CPSR)) #b0) ))
(and (= cond13 LS) (or (= ((_ extract 29 29) (select state13 CPSR)) #b0) (= ((_ extract 30 30) (select state13 CPSR)) #b1) ))
(and (= cond13 GE) (and (= ((_ extract 31 31) (select state13 CPSR)) #b1) (= ((_ extract 28 28) (select state13 CPSR)) #b1) ) )
(and (= cond13 LT) (or (= ((_ extract 31 31) (select state13 CPSR)) #b0) (= ((_ extract 28 28) (select state13 CPSR)) #b0) ) )
(and 
(= cond13 GT) 
(and 
(= ((_ extract 30 30) (select state13 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state13 CPSR)) #b1) (= ((_ extract 28 28) (select state13 CPSR)) #b1)) 
)
)
(and 
(= cond13 LE)
(or 
(= ((_ extract 30 30) (select state13 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state13 CPSR)) #b1) (= ((_ extract 28 28) (select state13 CPSR)) #b1))) )
)
(= cond13 AL)
)
(or
; execute_compare
(and
(or (= oper13 CMP)) 
; R0 to LR are equal
(= (select state13 R0) (select state14 R0)) 
(= (select state13 R1) (select state14 R1))
(= (select state13 R2) (select state14 R2))
(= (select state13 R3) (select state14 R3))
(= (select state13 R4) (select state14 R4))
(= (select state13 R5) (select state14 R5))
(= (select state13 R6) (select state14 R6))
(= (select state13 R7) (select state14 R7))
(= (select state13 R8) (select state14 R8))
(= (select state13 R9) (select state14 R9))
(= (select state13 R10) (select state14 R10))
(= (select state13 R11) (select state14 R11))
(= (select state13 R12) (select state14 R12))
(= (select state13 SP) (select state14 SP))
(= (select state13 LR) (select state14 LR))
(or (not (= oper13 CMP)) 
(ite (= rn13_val flex_val)
(= ((_ extract 30 30) (select state14 CPSR)) #b1) ; Z flag13 is 1 
(= ((_ extract 30 30) (select state14 CPSR)) #b0) ; Z flag13 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state13 R0) (select state14 R0)) (= rd13 R0))  
(or (= (select state13 R1) (select state14 R1)) (= rd13 R1))
(or (= (select state13 R2) (select state14 R2)) (= rd13 R2))
(or (= (select state13 R3) (select state14 R3)) (= rd13 R3))
(or (= (select state13 R4) (select state14 R4)) (= rd13 R4))
(or (= (select state13 R5) (select state14 R5)) (= rd13 R5))
(or (= (select state13 R6) (select state14 R6)) (= rd13 R6))
(or (= (select state13 R7) (select state14 R7)) (= rd13 R7))
(or (= (select state13 R8) (select state14 R8)) (= rd13 R8))
(or (= (select state13 R9) (select state14 R9)) (= rd13 R9))
(or (= (select state13 R10) (select state14 R10)) (= rd13 R10))
(or (= (select state13 R11) (select state14 R11)) (= rd13 R11))
(or (= (select state13 R12) (select state14 R12)) (= rd13 R12))
(or (= (select state13 SP) (select state14 SP)) (= rd13 SP))
(or (= (select state13 LR) (select state14 LR)) (= rd13 LR))
(or
; execute_2
(and
(or (= oper13 MOV) (= oper13 MVN))
(or (not (= oper13 MOV)) (= rd13_val flex_val))
(or (not (= oper13 MVN)) (= rd13_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper13 ADD) (= oper13 SUB) (= oper13 AND)) 
(or (not (= oper13 ADD)) (= rd13_val (bvadd rn13_val flex_val)))
(or (not (= oper13 SUB)) (= rd13_val (bvsub rn13_val flex_val)))
(or (not (= oper13 AND)) (= rd13_val (bvand rn13_val flex_val)))
)
)
; set flag13s or not
(or 
(and (= flag13 N) (= (select state13 CPSR) (select state14 CPSR))) 
(and (= flag13 S) 
(ite 
(= (select state14 rd13) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state14 CPSR)) #b1) ;Z flag13 is 1
(= ((_ extract 30 30) (select state14 CPSR)) #b0) ;Z flag13 is 0
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
(assert (=> (transition state14 state15)
(and
; pc incremented
(= (select state15 PC) (bvadd (select state14 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond14ition is false
(not
(or
(and (= cond14 EQ) (= ((_ extract 30 30) (select state14 CPSR)) #b1))
(and (= cond14 NE) (= ((_ extract 30 30) (select state14 CPSR)) #b0))
(and (= cond14 CS) (= ((_ extract 29 29) (select state14 CPSR)) #b1))
(and (= cond14 CC) (= ((_ extract 29 29) (select state14 CPSR)) #b0))
(and (= cond14 MI) (= ((_ extract 31 31) (select state14 CPSR)) #b1))
(and (= cond14 PL) (= ((_ extract 31 31) (select state14 CPSR)) #b0))
(and (= cond14 VS) (= ((_ extract 28 28) (select state14 CPSR)) #b1))
(and (= cond14 VC) (= ((_ extract 28 28) (select state14 CPSR)) #b0))
(and (= cond14 HI) (and (= ((_ extract 29 29) (select state14 CPSR)) #b1) (= ((_ extract 30 30) (select state14 CPSR)) #b0) ))
(and (= cond14 LS) (or (= ((_ extract 29 29) (select state14 CPSR)) #b0) (= ((_ extract 30 30) (select state14 CPSR)) #b1) ))
(and (= cond14 GE) (and (= ((_ extract 31 31) (select state14 CPSR)) #b1) (= ((_ extract 28 28) (select state14 CPSR)) #b1) ) )
(and (= cond14 LT) (or (= ((_ extract 31 31) (select state14 CPSR)) #b0) (= ((_ extract 28 28) (select state14 CPSR)) #b0) ) )
(and 
(= cond14 GT)
(and 
(= ((_ extract 30 30) (select state14 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state14 CPSR)) #b1) (= ((_ extract 28 28) (select state14 CPSR)) #b1)) 
)
)
(and 
(= cond14 LE)
(or 
(= ((_ extract 30 30) (select state14 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state14 CPSR)) #b1) (= ((_ extract 28 28) (select state14 CPSR)) #b1))) )
)
(= cond14 AL)
)
) 
; R0 to CPSR are equal
(= (select state14 R0) (select state15 R0)) 
(= (select state14 R1) (select state15 R1))
(= (select state14 R2) (select state15 R2))
(= (select state14 R3) (select state15 R3))
(= (select state14 R4) (select state15 R4))
(= (select state14 R5) (select state15 R5))
(= (select state14 R6) (select state15 R6))
(= (select state14 R7) (select state15 R7))
(= (select state14 R8) (select state15 R8))
(= (select state14 R9) (select state15 R9))
(= (select state14 R10) (select state15 R10))
(= (select state14 R11) (select state15 R11))
(= (select state14 R12) (select state15 R12))
(= (select state14 SP) (select state15 SP))
(= (select state14 LR) (select state15 LR)) 
(= (select state14 CPSR) (select state15 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm14_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm14))) (select state14 ro14))))
(ite
(= barrel_op14 LSL)
(bvshl val_to_shift barrel_num14)
(ite 
(= barrel_op14 LSR)
(bvlshr val_to_shift barrel_num14)
(bvashr val_to_shift barrel_num14)
)
)
)
)
(rd14_val (select state15 rd14)) (rn14_val (select state14 rn14))
)
; instruction is executed
(and
; cond14ition is true
(or
(and (= cond14 EQ) (= ((_ extract 30 30) (select state14 CPSR)) #b1))
(and (= cond14 NE) (= ((_ extract 30 30) (select state14 CPSR)) #b0))
(and (= cond14 CS) (= ((_ extract 29 29) (select state14 CPSR)) #b1))
(and (= cond14 CC) (= ((_ extract 29 29) (select state14 CPSR)) #b0))
(and (= cond14 MI) (= ((_ extract 31 31) (select state14 CPSR)) #b1))
(and (= cond14 PL) (= ((_ extract 31 31) (select state14 CPSR)) #b0))
(and (= cond14 VS) (= ((_ extract 28 28) (select state14 CPSR)) #b1))
(and (= cond14 VC) (= ((_ extract 28 28) (select state14 CPSR)) #b0))
(and (= cond14 HI) (and (= ((_ extract 29 29) (select state14 CPSR)) #b1) (= ((_ extract 30 30) (select state14 CPSR)) #b0) ))
(and (= cond14 LS) (or (= ((_ extract 29 29) (select state14 CPSR)) #b0) (= ((_ extract 30 30) (select state14 CPSR)) #b1) ))
(and (= cond14 GE) (and (= ((_ extract 31 31) (select state14 CPSR)) #b1) (= ((_ extract 28 28) (select state14 CPSR)) #b1) ) )
(and (= cond14 LT) (or (= ((_ extract 31 31) (select state14 CPSR)) #b0) (= ((_ extract 28 28) (select state14 CPSR)) #b0) ) )
(and 
(= cond14 GT) 
(and 
(= ((_ extract 30 30) (select state14 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state14 CPSR)) #b1) (= ((_ extract 28 28) (select state14 CPSR)) #b1)) 
)
)
(and 
(= cond14 LE)
(or 
(= ((_ extract 30 30) (select state14 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state14 CPSR)) #b1) (= ((_ extract 28 28) (select state14 CPSR)) #b1))) )
)
(= cond14 AL)
)
(or
; execute_compare
(and
(or (= oper14 CMP)) 
; R0 to LR are equal
(= (select state14 R0) (select state15 R0)) 
(= (select state14 R1) (select state15 R1))
(= (select state14 R2) (select state15 R2))
(= (select state14 R3) (select state15 R3))
(= (select state14 R4) (select state15 R4))
(= (select state14 R5) (select state15 R5))
(= (select state14 R6) (select state15 R6))
(= (select state14 R7) (select state15 R7))
(= (select state14 R8) (select state15 R8))
(= (select state14 R9) (select state15 R9))
(= (select state14 R10) (select state15 R10))
(= (select state14 R11) (select state15 R11))
(= (select state14 R12) (select state15 R12))
(= (select state14 SP) (select state15 SP))
(= (select state14 LR) (select state15 LR))
(or (not (= oper14 CMP)) 
(ite (= rn14_val flex_val)
(= ((_ extract 30 30) (select state15 CPSR)) #b1) ; Z flag14 is 1 
(= ((_ extract 30 30) (select state15 CPSR)) #b0) ; Z flag14 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state14 R0) (select state15 R0)) (= rd14 R0))  
(or (= (select state14 R1) (select state15 R1)) (= rd14 R1))
(or (= (select state14 R2) (select state15 R2)) (= rd14 R2))
(or (= (select state14 R3) (select state15 R3)) (= rd14 R3))
(or (= (select state14 R4) (select state15 R4)) (= rd14 R4))
(or (= (select state14 R5) (select state15 R5)) (= rd14 R5))
(or (= (select state14 R6) (select state15 R6)) (= rd14 R6))
(or (= (select state14 R7) (select state15 R7)) (= rd14 R7))
(or (= (select state14 R8) (select state15 R8)) (= rd14 R8))
(or (= (select state14 R9) (select state15 R9)) (= rd14 R9))
(or (= (select state14 R10) (select state15 R10)) (= rd14 R10))
(or (= (select state14 R11) (select state15 R11)) (= rd14 R11))
(or (= (select state14 R12) (select state15 R12)) (= rd14 R12))
(or (= (select state14 SP) (select state15 SP)) (= rd14 SP))
(or (= (select state14 LR) (select state15 LR)) (= rd14 LR))
(or
; execute_2
(and
(or (= oper14 MOV) (= oper14 MVN))
(or (not (= oper14 MOV)) (= rd14_val flex_val))
(or (not (= oper14 MVN)) (= rd14_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper14 ADD) (= oper14 SUB) (= oper14 AND)) 
(or (not (= oper14 ADD)) (= rd14_val (bvadd rn14_val flex_val)))
(or (not (= oper14 SUB)) (= rd14_val (bvsub rn14_val flex_val)))
(or (not (= oper14 AND)) (= rd14_val (bvand rn14_val flex_val)))
)
)
; set flag14s or not
(or 
(and (= flag14 N) (= (select state14 CPSR) (select state15 CPSR))) 
(and (= flag14 S) 
(ite 
(= (select state15 rd14) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state15 CPSR)) #b1) ;Z flag14 is 1
(= ((_ extract 30 30) (select state15 CPSR)) #b0) ;Z flag14 is 0
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
(assert (=> (transition state15 state16)
(and
; pc incremented
(= (select state16 PC) (bvadd (select state15 PC) #b00000000000000000000000000000100))
(or 
; instruction not executed
(and 
; cond15ition is false
(not
(or
(and (= cond15 EQ) (= ((_ extract 30 30) (select state15 CPSR)) #b1))
(and (= cond15 NE) (= ((_ extract 30 30) (select state15 CPSR)) #b0))
(and (= cond15 CS) (= ((_ extract 29 29) (select state15 CPSR)) #b1))
(and (= cond15 CC) (= ((_ extract 29 29) (select state15 CPSR)) #b0))
(and (= cond15 MI) (= ((_ extract 31 31) (select state15 CPSR)) #b1))
(and (= cond15 PL) (= ((_ extract 31 31) (select state15 CPSR)) #b0))
(and (= cond15 VS) (= ((_ extract 28 28) (select state15 CPSR)) #b1))
(and (= cond15 VC) (= ((_ extract 28 28) (select state15 CPSR)) #b0))
(and (= cond15 HI) (and (= ((_ extract 29 29) (select state15 CPSR)) #b1) (= ((_ extract 30 30) (select state15 CPSR)) #b0) ))
(and (= cond15 LS) (or (= ((_ extract 29 29) (select state15 CPSR)) #b0) (= ((_ extract 30 30) (select state15 CPSR)) #b1) ))
(and (= cond15 GE) (and (= ((_ extract 31 31) (select state15 CPSR)) #b1) (= ((_ extract 28 28) (select state15 CPSR)) #b1) ) )
(and (= cond15 LT) (or (= ((_ extract 31 31) (select state15 CPSR)) #b0) (= ((_ extract 28 28) (select state15 CPSR)) #b0) ) )
(and 
(= cond15 GT)
(and 
(= ((_ extract 30 30) (select state15 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state15 CPSR)) #b1) (= ((_ extract 28 28) (select state15 CPSR)) #b1)) 
)
)
(and 
(= cond15 LE)
(or 
(= ((_ extract 30 30) (select state15 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state15 CPSR)) #b1) (= ((_ extract 28 28) (select state15 CPSR)) #b1))) )
)
(= cond15 AL)
)
) 
; R0 to CPSR are equal
(= (select state15 R0) (select state16 R0)) 
(= (select state15 R1) (select state16 R1))
(= (select state15 R2) (select state16 R2))
(= (select state15 R3) (select state16 R3))
(= (select state15 R4) (select state16 R4))
(= (select state15 R5) (select state16 R5))
(= (select state15 R6) (select state16 R6))
(= (select state15 R7) (select state16 R7))
(= (select state15 R8) (select state16 R8))
(= (select state15 R9) (select state16 R9))
(= (select state15 R10) (select state16 R10))
(= (select state15 R11) (select state16 R11))
(= (select state15 R12) (select state16 R12))
(= (select state15 SP) (select state16 SP))
(= (select state15 LR) (select state16 LR)) 
(= (select state15 CPSR) (select state16 CPSR)) 
)
(let 
((flex_val 
(let ((val_to_shift (ite imm15_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm15))) (select state15 ro15))))
(ite
(= barrel_op15 LSL)
(bvshl val_to_shift barrel_num15)
(ite 
(= barrel_op15 LSR)
(bvlshr val_to_shift barrel_num15)
(bvashr val_to_shift barrel_num15)
)
)
)
)
(rd15_val (select state16 rd15)) (rn15_val (select state15 rn15))
)
; instruction is executed
(and
; cond15ition is true
(or
(and (= cond15 EQ) (= ((_ extract 30 30) (select state15 CPSR)) #b1))
(and (= cond15 NE) (= ((_ extract 30 30) (select state15 CPSR)) #b0))
(and (= cond15 CS) (= ((_ extract 29 29) (select state15 CPSR)) #b1))
(and (= cond15 CC) (= ((_ extract 29 29) (select state15 CPSR)) #b0))
(and (= cond15 MI) (= ((_ extract 31 31) (select state15 CPSR)) #b1))
(and (= cond15 PL) (= ((_ extract 31 31) (select state15 CPSR)) #b0))
(and (= cond15 VS) (= ((_ extract 28 28) (select state15 CPSR)) #b1))
(and (= cond15 VC) (= ((_ extract 28 28) (select state15 CPSR)) #b0))
(and (= cond15 HI) (and (= ((_ extract 29 29) (select state15 CPSR)) #b1) (= ((_ extract 30 30) (select state15 CPSR)) #b0) ))
(and (= cond15 LS) (or (= ((_ extract 29 29) (select state15 CPSR)) #b0) (= ((_ extract 30 30) (select state15 CPSR)) #b1) ))
(and (= cond15 GE) (and (= ((_ extract 31 31) (select state15 CPSR)) #b1) (= ((_ extract 28 28) (select state15 CPSR)) #b1) ) )
(and (= cond15 LT) (or (= ((_ extract 31 31) (select state15 CPSR)) #b0) (= ((_ extract 28 28) (select state15 CPSR)) #b0) ) )
(and 
(= cond15 GT) 
(and 
(= ((_ extract 30 30) (select state15 CPSR)) #b0) 
(= (= ((_ extract 31 31) (select state15 CPSR)) #b1) (= ((_ extract 28 28) (select state15 CPSR)) #b1)) 
)
)
(and 
(= cond15 LE)
(or 
(= ((_ extract 30 30) (select state15 CPSR)) #b1) 
(not(= (= ((_ extract 31 31) (select state15 CPSR)) #b1) (= ((_ extract 28 28) (select state15 CPSR)) #b1))) )
)
(= cond15 AL)
)
(or
; execute_compare
(and
(or (= oper15 CMP)) 
; R0 to LR are equal
(= (select state15 R0) (select state16 R0)) 
(= (select state15 R1) (select state16 R1))
(= (select state15 R2) (select state16 R2))
(= (select state15 R3) (select state16 R3))
(= (select state15 R4) (select state16 R4))
(= (select state15 R5) (select state16 R5))
(= (select state15 R6) (select state16 R6))
(= (select state15 R7) (select state16 R7))
(= (select state15 R8) (select state16 R8))
(= (select state15 R9) (select state16 R9))
(= (select state15 R10) (select state16 R10))
(= (select state15 R11) (select state16 R11))
(= (select state15 R12) (select state16 R12))
(= (select state15 SP) (select state16 SP))
(= (select state15 LR) (select state16 LR))
(or (not (= oper15 CMP)) 
(ite (= rn15_val flex_val)
(= ((_ extract 30 30) (select state16 CPSR)) #b1) ; Z flag15 is 1 
(= ((_ extract 30 30) (select state16 CPSR)) #b0) ; Z flag15 is 0 
)
)	
)
(and
; R0 to LR equal except destination register
(or (= (select state15 R0) (select state16 R0)) (= rd15 R0))  
(or (= (select state15 R1) (select state16 R1)) (= rd15 R1))
(or (= (select state15 R2) (select state16 R2)) (= rd15 R2))
(or (= (select state15 R3) (select state16 R3)) (= rd15 R3))
(or (= (select state15 R4) (select state16 R4)) (= rd15 R4))
(or (= (select state15 R5) (select state16 R5)) (= rd15 R5))
(or (= (select state15 R6) (select state16 R6)) (= rd15 R6))
(or (= (select state15 R7) (select state16 R7)) (= rd15 R7))
(or (= (select state15 R8) (select state16 R8)) (= rd15 R8))
(or (= (select state15 R9) (select state16 R9)) (= rd15 R9))
(or (= (select state15 R10) (select state16 R10)) (= rd15 R10))
(or (= (select state15 R11) (select state16 R11)) (= rd15 R11))
(or (= (select state15 R12) (select state16 R12)) (= rd15 R12))
(or (= (select state15 SP) (select state16 SP)) (= rd15 SP))
(or (= (select state15 LR) (select state16 LR)) (= rd15 LR))
(or
; execute_2
(and
(or (= oper15 MOV) (= oper15 MVN))
(or (not (= oper15 MOV)) (= rd15_val flex_val))
(or (not (= oper15 MVN)) (= rd15_val (bvnot flex_val)))
)
; execute_3
(and
(or (= oper15 ADD) (= oper15 SUB) (= oper15 AND)) 
(or (not (= oper15 ADD)) (= rd15_val (bvadd rn15_val flex_val)))
(or (not (= oper15 SUB)) (= rd15_val (bvsub rn15_val flex_val)))
(or (not (= oper15 AND)) (= rd15_val (bvand rn15_val flex_val)))
)
)
; set flag15s or not
(or 
(and (= flag15 N) (= (select state15 CPSR) (select state16 CPSR))) 
(and (= flag15 S) 
(ite 
(= (select state16 rd15) #b00000000000000000000000000000000) 
(= ((_ extract 30 30) (select state16 CPSR)) #b1) ;Z flag15 is 1
(= ((_ extract 30 30) (select state16 CPSR)) #b0) ;Z flag15 is 0
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
(assert (transition state0 state1))
(assert (transition state1 state2))
(assert (transition state2 state3))
(assert (transition state3 state4))
(assert (transition state4 state5))
(assert (transition state5 state6))
(assert (transition state6 state7))
(assert (transition state7 state8))
(assert (transition state8 state9))
(assert (transition state9 state10))
(assert (transition state10 state11))
(assert (transition state11 state12))
(assert (transition state12 state13))
(assert (transition state13 state14))
(assert (transition state14 state15))
(assert (transition state15 state16))
(assert (= (select state0 R0) #b00000000000000000000000000000000))
(assert (= (select state0 R1) #b00000000000000000000000000001111))
(assert (= (select state0 R2) #b00000000000000000000000000000000))
(assert (= (select state0 R3) #b00000000000000000000000000000000))
(assert (= (select state0 R4) #b00000000000000000000000000000000))
(assert (= (select state0 R5) #b00000000000000000000000000000000))
(assert (= (select state0 R6) #b00000000000000000000000000000000))
(assert (= (select state0 R7) #b00000000000000000000000000000000))
(assert (= (select state0 R8) #b00000000000000000000000000000000))
(assert (= (select state0 R9) #b00000000000000000000000000000000))
(assert (= (select state0 R10) #b00000000000000000000000000000000))
(assert (= (select state0 R11) #b00000000000000000000000000000000))
(assert (= (select state0 R12) #b00000000000000000000000000000000))
(assert (= (select state0 SP) #b00000000000000000000000000000000))
(assert (= (select state0 LR) #b00000000000000000000000000000000))
(assert (= (select state0 PC) #b00000000000000000000000000000000))
(assert (= (select state0 CPSR) #b01000000000000000000000000000000))
(assert (= (select state16 R0) #b00000000000011000000000001100000))
(assert (= (select state16 R1) #b00100000000000000000000000001111))
(assert (= (select state16 R2) #b00000000000000000000000000000000))
(assert (= (select state16 R3) #b00000000001100000000000000000000))
(assert (= (select state16 R4) #b00000000000000000000000000000000))
(assert (= (select state16 R5) #b00001111000000000000001100000000))
(assert (= (select state16 R6) #b00000000000000000000000000000000))
(assert (= (select state16 R7) #b00000001100000000000000000000000))
(assert (= (select state16 R8) #b00000000000000000000000000000000))
(assert (= (select state16 R9) #b00000000000000000000001111110000))
(assert (= (select state16 R10) #b00000000110000000000000000000000))
(assert (= (select state16 R11) #b00000000000000000000000000000000))
(assert (= (select state16 R12) #b00000000000011110000000000000000))
(assert (= (select state16 SP) #b00000000000000000000000000000000))
(assert (= (select state16 LR) #b00000000000000000000000000000000))
(assert (= (select state16 PC) #b00000000000000000000000001000000))
(assert (= (select state16 CPSR) #b00000000000000000000000000000000))
(check-sat)
(echo "")
(eval oper0)
(eval cond0)
(eval flag0)
(eval rd0)
(eval rn0)
(eval ro0)
(eval imm0_used)
(eval imm0)
(eval barrel_op0)
(eval barrel_num0)
(echo "")
(eval oper1)
(eval cond1)
(eval flag1)
(eval rd1)
(eval rn1)
(eval ro1)
(eval imm1_used)
(eval imm1)
(eval barrel_op1)
(eval barrel_num1)
(echo "")
(eval oper2)
(eval cond2)
(eval flag2)
(eval rd2)
(eval rn2)
(eval ro2)
(eval imm2_used)
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
(eval imm3_used)
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
(eval imm4_used)
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
(eval imm5_used)
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
(eval imm6_used)
(eval imm6)
(eval barrel_op6)
(eval barrel_num6)
(echo "")
(eval oper7)
(eval cond7)
(eval flag7)
(eval rd7)
(eval rn7)
(eval ro7)
(eval imm7_used)
(eval imm7)
(eval barrel_op7)
(eval barrel_num7)
(echo "")
(eval oper8)
(eval cond8)
(eval flag8)
(eval rd8)
(eval rn8)
(eval ro8)
(eval imm8_used)
(eval imm8)
(eval barrel_op8)
(eval barrel_num8)
(echo "")
(eval oper9)
(eval cond9)
(eval flag9)
(eval rd9)
(eval rn9)
(eval ro9)
(eval imm9_used)
(eval imm9)
(eval barrel_op9)
(eval barrel_num9)
(echo "")
(eval oper10)
(eval cond10)
(eval flag10)
(eval rd10)
(eval rn10)
(eval ro10)
(eval imm10_used)
(eval imm10)
(eval barrel_op10)
(eval barrel_num10)
(echo "")
(eval oper11)
(eval cond11)
(eval flag11)
(eval rd11)
(eval rn11)
(eval ro11)
(eval imm11_used)
(eval imm11)
(eval barrel_op11)
(eval barrel_num11)
(echo "")
(eval oper12)
(eval cond12)
(eval flag12)
(eval rd12)
(eval rn12)
(eval ro12)
(eval imm12_used)
(eval imm12)
(eval barrel_op12)
(eval barrel_num12)
(echo "")
(eval oper13)
(eval cond13)
(eval flag13)
(eval rd13)
(eval rn13)
(eval ro13)
(eval imm13_used)
(eval imm13)
(eval barrel_op13)
(eval barrel_num13)
(echo "")
(eval oper14)
(eval cond14)
(eval flag14)
(eval rd14)
(eval rn14)
(eval ro14)
(eval imm14_used)
(eval imm14)
(eval barrel_op14)
(eval barrel_num14)
(echo "")
(eval oper15)
(eval cond15)
(eval flag15)
(eval rd15)
(eval rn15)
(eval ro15)
(eval imm15_used)
(eval imm15)
(eval barrel_op15)
(eval barrel_num15)
