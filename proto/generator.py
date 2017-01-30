out = open("proto_gen.smt2", "w") 

out.write("(echo \"Generating ARM V8 Constraints\")\r\n")

out.write("(declare-datatypes () ((Operation ADD SUB MOV MVN CMP AND)))\r\n")
out.write("(declare-datatypes () ((Condition EQ NE CS CC MI PL VS VC HI LS GE LT GT LE AL)))\r\n")
out.write("(declare-datatypes () ((Flag N S)))\r\n")
out.write("(declare-datatypes () ((BarrelOp LSL LSR ASR)))\r\n")
out.write("(declare-datatypes () ((Register R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 SP LR PC CPSR)))\r\n")
out.write("(define-sort State () (Array Register (_ BitVec 32)))\r\n")

out.write("(declare-rel transition (State State))\r\n")

cycles = 17

for i in range(0, cycles):
	out.write("(declare-var state" + str(i) + " State)\r\n")
	out.write("(declare-var oper" + str(i) + " Operation)\r\n")
	out.write("(declare-var cond" + str(i) + " Condition)\r\n")
	out.write("(declare-var flag"+ str(i) + " Flag)\r\n")
	out.write("(declare-var rd" + str(i) + " Register)\r\n")
	out.write("(declare-var rn" + str(i) + " Register)\r\n")
	out.write("(declare-var ro"+ str(i) + " Register)\r\n")
	out.write("(declare-var imm"+ str(i) + " (_ BitVec 12))\r\n")
	out.write("(declare-var imm"+ str(i) +"_used Bool)\r\n")
	out.write("(declare-var barrel_op" + str(i) + " BarrelOp)\r\n")
	out.write("(declare-var barrel_num"+str(i) + " (_ BitVec 32))\r\n")


for i in range(0, cycles - 1):
	out.write("(assert (=> (transition state"+ str(i) +" state"+ str(i+1) +")\r\n")
	out.write("(and\r\n")
	out.write("; pc incremented\r\n")
	out.write("(= (select state"+ str(i+1)+" PC) (bvadd (select state"+ str(i) +" PC) #b00000000000000000000000000000100))\r\n")
	out.write("(or \r\n")
	out.write("; instruction not executed\r\n")
	out.write("(and \r\n")
	out.write("; cond"+ str(i) +"ition is false\r\n")
	out.write("(not\r\n")
	out.write("(or\r\n")
	out.write("(and (= cond"+ str(i) +" EQ) (= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b1))\r\n")
	out.write("(and (= cond"+ str(i) +" NE) (= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b0))\r\n")
	out.write("(and (= cond"+ str(i) +" CS) (= ((_ extract 29 29) (select state"+ str(i) +" CPSR)) #b1))\r\n")
	out.write("(and (= cond"+ str(i) +" CC) (= ((_ extract 29 29) (select state"+ str(i) +" CPSR)) #b0))\r\n")
	out.write("(and (= cond"+ str(i) +" MI) (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b1))\r\n")
	out.write("(and (= cond"+ str(i) +" PL) (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b0))\r\n")
	out.write("(and (= cond"+ str(i) +" VS) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b1))\r\n")
	out.write("(and (= cond"+ str(i) +" VC) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b0))\r\n")

	out.write("(and (= cond"+ str(i) +" HI) (and (= ((_ extract 29 29) (select state"+ str(i) +" CPSR)) #b1) (= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b0) ))\r\n")
	out.write("(and (= cond"+ str(i) +" LS) (or (= ((_ extract 29 29) (select state"+ str(i) +" CPSR)) #b0) (= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b1) ))\r\n")
	out.write("(and (= cond"+ str(i) +" GE) (and (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b1) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b1) ) )\r\n")
	out.write("(and (= cond"+ str(i) +" LT) (or (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b0) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b0) ) )\r\n")
	out.write("(and \r\n")
	out.write("(= cond"+ str(i) +" GT)\r\n") 
	out.write("(and \r\n")
	out.write("(= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b0) \r\n")
	out.write("(= (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b1) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b1)) \r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write("(and \r\n")
	out.write("(= cond"+ str(i) +" LE)\r\n") 
	out.write("(or \r\n")
	out.write("(= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b1) \r\n")
	out.write("(not(= (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b1) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b1))) )\r\n")
	out.write(")\r\n")
	out.write("(= cond"+ str(i) +" AL)\r\n")
	out.write(")\r\n")
	out.write(") \r\n")
	out.write("; R0 to CPSR are equal\r\n")
	out.write("(= (select state"+ str(i) +" R0) (select state"+ str(i+1) +" R0)) \r\n")
	out.write("(= (select state"+ str(i) +" R1) (select state"+ str(i+1) +" R1))\r\n")
	out.write("(= (select state"+ str(i) +" R2) (select state"+ str(i+1) +" R2))\r\n")
	out.write("(= (select state"+ str(i) +" R3) (select state"+ str(i+1) +" R3))\r\n")
	out.write("(= (select state"+ str(i) +" R4) (select state"+ str(i+1) +" R4))\r\n")
	out.write("(= (select state"+ str(i) +" R5) (select state"+ str(i+1) +" R5))\r\n")
	out.write("(= (select state"+ str(i) +" R6) (select state"+ str(i+1) +" R6))\r\n")
	out.write("(= (select state"+ str(i) +" R7) (select state"+ str(i+1) +" R7))\r\n")
	out.write("(= (select state"+ str(i) +" R8) (select state"+ str(i+1) +" R8))\r\n")
	out.write("(= (select state"+ str(i) +" R9) (select state"+ str(i+1) +" R9))\r\n")
	out.write("(= (select state"+ str(i) +" R10) (select state"+ str(i+1) +" R10))\r\n")
	out.write("(= (select state"+ str(i) +" R11) (select state"+ str(i+1) +" R11))\r\n")
	out.write("(= (select state"+ str(i) +" R12) (select state"+ str(i+1) +" R12))\r\n")
	out.write("(= (select state"+ str(i) +" SP) (select state"+ str(i+1) +" SP))\r\n")
	out.write("(= (select state"+ str(i) +" LR) (select state"+ str(i+1) +" LR)) \r\n")
	out.write("(= (select state"+ str(i) +" CPSR) (select state"+ str(i+1) +" CPSR)) \r\n")
	out.write(")\r\n")
	out.write("(let \r\n")
	out.write("((flex_val \r\n")
	out.write("(let ((val_to_shift (ite imm"+ str(i) +"_used ((_ rotate_right 0) (concat #b000000000000000000000000 (( _ extract 7 0) imm"+ str(i) +"))) (select state"+ str(i) +" ro"+ str(i) +"))))\r\n")
	out.write("(ite\r\n")
	out.write("(= barrel_op"+ str(i) +" LSL)\r\n")
	out.write("(bvshl val_to_shift barrel_num"+ str(i) +")\r\n")
	out.write("(ite \r\n")
	out.write("(= barrel_op"+ str(i) +" LSR)\r\n")
	out.write("(bvlshr val_to_shift barrel_num"+ str(i) +")\r\n")
	out.write("(bvashr val_to_shift barrel_num"+ str(i) +")\r\n")
	out.write(")\r\n")

	out.write(")\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write("(rd"+ str(i) +"_val (select state"+ str(i+1) +" rd"+ str(i) +")) (rn"+ str(i) +"_val (select state"+ str(i) +" rn"+ str(i) +"))\r\n")
	out.write(")\r\n")

	out.write("; instruction is executed\r\n")
	out.write("(and\r\n")
	out.write("; cond"+ str(i) +"ition is true\r\n")
	out.write("(or\r\n")
	out.write("(and (= cond"+ str(i) +" EQ) (= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b1))\r\n")
	out.write("(and (= cond"+ str(i) +" NE) (= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b0))\r\n")
	out.write("(and (= cond"+ str(i) +" CS) (= ((_ extract 29 29) (select state"+ str(i) +" CPSR)) #b1))\r\n")
	out.write("(and (= cond"+ str(i) +" CC) (= ((_ extract 29 29) (select state"+ str(i) +" CPSR)) #b0))\r\n")
	out.write("(and (= cond"+ str(i) +" MI) (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b1))\r\n")
	out.write("(and (= cond"+ str(i) +" PL) (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b0))\r\n")
	out.write("(and (= cond"+ str(i) +" VS) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b1))\r\n")
	out.write("(and (= cond"+ str(i) +" VC) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b0))\r\n")

	out.write("(and (= cond"+ str(i) +" HI) (and (= ((_ extract 29 29) (select state"+ str(i) +" CPSR)) #b1) (= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b0) ))\r\n")
	out.write("(and (= cond"+ str(i) +" LS) (or (= ((_ extract 29 29) (select state"+ str(i) +" CPSR)) #b0) (= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b1) ))\r\n")
	out.write("(and (= cond"+ str(i) +" GE) (and (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b1) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b1) ) )\r\n")
	out.write("(and (= cond"+ str(i) +" LT) (or (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b0) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b0) ) )\r\n")
	out.write("(and \r\n")
	out.write("(= cond"+ str(i) +" GT) \r\n")
	out.write("(and \r\n")
	out.write("(= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b0) \r\n")
	out.write("(= (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b1) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b1)) \r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write("(and \r\n")
	out.write("(= cond"+ str(i) +" LE)\r\n") 
	out.write("(or \r\n")
	out.write("(= ((_ extract 30 30) (select state"+ str(i) +" CPSR)) #b1) \r\n")
	out.write("(not(= (= ((_ extract 31 31) (select state"+ str(i) +" CPSR)) #b1) (= ((_ extract 28 28) (select state"+ str(i) +" CPSR)) #b1))) )\r\n")
	out.write(")\r\n")
	out.write("(= cond"+ str(i) +" AL)\r\n")
	out.write(")\r\n")
	out.write("(or\r\n")
	out.write("; execute_compare\r\n")
	out.write("(and\r\n")
	out.write("(or (= oper"+ str(i) +" CMP)) \r\n")
	out.write("; R0 to LR are equal\r\n")
	out.write("(= (select state"+ str(i) +" R0) (select state"+ str(i+1) +" R0)) \r\n")
	out.write("(= (select state"+ str(i) +" R1) (select state"+ str(i+1) +" R1))\r\n")
	out.write("(= (select state"+ str(i) +" R2) (select state"+ str(i+1) +" R2))\r\n")
	out.write("(= (select state"+ str(i) +" R3) (select state"+ str(i+1) +" R3))\r\n")
	out.write("(= (select state"+ str(i) +" R4) (select state"+ str(i+1) +" R4))\r\n")
	out.write("(= (select state"+ str(i) +" R5) (select state"+ str(i+1) +" R5))\r\n")
	out.write("(= (select state"+ str(i) +" R6) (select state"+ str(i+1) +" R6))\r\n")
	out.write("(= (select state"+ str(i) +" R7) (select state"+ str(i+1) +" R7))\r\n")
	out.write("(= (select state"+ str(i) +" R8) (select state"+ str(i+1) +" R8))\r\n")
	out.write("(= (select state"+ str(i) +" R9) (select state"+ str(i+1) +" R9))\r\n")
	out.write("(= (select state"+ str(i) +" R10) (select state"+ str(i+1) +" R10))\r\n")
	out.write("(= (select state"+ str(i) +" R11) (select state"+ str(i+1) +" R11))\r\n")
	out.write("(= (select state"+ str(i) +" R12) (select state"+ str(i+1) +" R12))\r\n")
	out.write("(= (select state"+ str(i) +" SP) (select state"+ str(i+1) +" SP))\r\n")
	out.write("(= (select state"+ str(i) +" LR) (select state"+ str(i+1) +" LR))\r\n")

	out.write("(or (not (= oper"+ str(i) +" CMP)) \r\n")
	out.write("(ite (= rn"+ str(i) +"_val flex_val)\r\n")
	out.write("(= ((_ extract 30 30) (select state"+ str(i+1) +" CPSR)) #b1) ; Z flag"+ str(i) +" is 1 \r\n")
	out.write("(= ((_ extract 30 30) (select state"+ str(i+1) +" CPSR)) #b0) ; Z flag"+ str(i) +" is 0 \r\n")
	out.write(")\r\n")
	out.write(")	\r\n")		
	out.write(")\r\n")
						
	out.write("(and\r\n")
	out.write("; R0 to LR equal except destination register\r\n")
	out.write("(or (= (select state"+ str(i) +" R0) (select state"+ str(i+1) +" R0)) (= rd"+ str(i) +" R0))  \r\n")
	out.write("(or (= (select state"+ str(i) +" R1) (select state"+ str(i+1) +" R1)) (= rd"+ str(i) +" R1))\r\n")
	out.write("(or (= (select state"+ str(i) +" R2) (select state"+ str(i+1) +" R2)) (= rd"+ str(i) +" R2))\r\n")
	out.write("(or (= (select state"+ str(i) +" R3) (select state"+ str(i+1) +" R3)) (= rd"+ str(i) +" R3))\r\n")
	out.write("(or (= (select state"+ str(i) +" R4) (select state"+ str(i+1) +" R4)) (= rd"+ str(i) +" R4))\r\n")
	out.write("(or (= (select state"+ str(i) +" R5) (select state"+ str(i+1) +" R5)) (= rd"+ str(i) +" R5))\r\n")
	out.write("(or (= (select state"+ str(i) +" R6) (select state"+ str(i+1) +" R6)) (= rd"+ str(i) +" R6))\r\n")
	out.write("(or (= (select state"+ str(i) +" R7) (select state"+ str(i+1) +" R7)) (= rd"+ str(i) +" R7))\r\n")
	out.write("(or (= (select state"+ str(i) +" R8) (select state"+ str(i+1) +" R8)) (= rd"+ str(i) +" R8))\r\n")
	out.write("(or (= (select state"+ str(i) +" R9) (select state"+ str(i+1) +" R9)) (= rd"+ str(i) +" R9))\r\n")
	out.write("(or (= (select state"+ str(i) +" R10) (select state"+ str(i+1) +" R10)) (= rd"+ str(i) +" R10))\r\n")
	out.write("(or (= (select state"+ str(i) +" R11) (select state"+ str(i+1) +" R11)) (= rd"+ str(i) +" R11))\r\n")
	out.write("(or (= (select state"+ str(i) +" R12) (select state"+ str(i+1) +" R12)) (= rd"+ str(i) +" R12))\r\n")
	out.write("(or (= (select state"+ str(i) +" SP) (select state"+ str(i+1) +" SP)) (= rd"+ str(i) +" SP))\r\n")
	out.write("(or (= (select state"+ str(i) +" LR) (select state"+ str(i+1) +" LR)) (= rd"+ str(i) +" LR))\r\n")

	out.write("(or\r\n")
	out.write("; execute_2\r\n")
	out.write("(and\r\n")
	out.write("(or (= oper"+ str(i) +" MOV) (= oper"+ str(i) +" MVN))\r\n")

	out.write("(or (not (= oper"+ str(i) +" MOV)) (= rd"+ str(i) +"_val flex_val))\r\n")
	out.write("(or (not (= oper"+ str(i) +" MVN)) (= rd"+ str(i) +"_val (bvnot flex_val)))\r\n")
	out.write(")\r\n")
	out.write("; execute_3\r\n")
	out.write("(and\r\n")
	out.write("(or (= oper"+ str(i) +" ADD) (= oper"+ str(i) +" SUB) (= oper"+ str(i) +" AND)) \r\n")

	out.write("(or (not (= oper"+ str(i) +" ADD)) (= rd"+ str(i) +"_val (bvadd rn"+ str(i) +"_val flex_val)))\r\n")
	out.write("(or (not (= oper"+ str(i) +" SUB)) (= rd"+ str(i) +"_val (bvsub rn"+ str(i) +"_val flex_val)))\r\n")
	out.write("(or (not (= oper"+ str(i) +" AND)) (= rd"+ str(i) +"_val (bvand rn"+ str(i) +"_val flex_val)))\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write("; set flag"+ str(i) +"s or not\r\n")
	out.write("(or \r\n")
	out.write("(and (= flag"+ str(i) +" N) (= (select state"+ str(i) +" CPSR) (select state"+ str(i+1) +" CPSR))) \r\n")
	out.write("(and (= flag"+ str(i) +" S) \r\n")
	out.write("(ite \r\n")
	out.write("(= (select state"+ str(i+1) +" rd"+ str(i) +") #b00000000000000000000000000000000) \r\n")
	out.write("(= ((_ extract 30 30) (select state"+ str(i+1) +" CPSR)) #b1) ;Z flag"+ str(i) +" is 1\r\n")
	out.write("(= ((_ extract 30 30) (select state"+ str(i+1) +" CPSR)) #b0) ;Z flag"+ str(i) +" is 0\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write(")\r\n")
	out.write("))\r\n")

for i in range(0, cycles - 1):
    out.write("(assert (transition state"+str(i)+" state"+str(i + 1)+"))\r\n")

out.write("(assert (= (select state"+ str(0) +" R0) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R1) #b00000000000000000000000000001111))\r\n")
out.write("(assert (= (select state"+ str(0) +" R2) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R3) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R4) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R5) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R6) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R7) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R8) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R9) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R10) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R11) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" R12) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" SP) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" LR) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" PC) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(0) +" CPSR) #b01000000000000000000000000000000))\r\n")

out.write("(assert (= (select state"+ str(cycles-1) +" R0) #b00000000000011000000000001100000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R1) #b00100000000000000000000000001111))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R2) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R3) #b00000000001100000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R4) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R5) #b00001111000000000000001100000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R6) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R7) #b00000001100000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R8) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R9) #b00000000000000000000001111110000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R10) #b00000000110000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R11) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" R12) #b00000000000011110000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" SP) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" LR) #b00000000000000000000000000000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" PC) #b00000000000000000000000001000000))\r\n")
out.write("(assert (= (select state"+ str(cycles-1) +" CPSR) #b00000000000000000000000000000000))\r\n")

out.write("(check-sat)\r\n")

#out.write("(get-model)\r\n")

for i in range(0, cycles - 1):
	out.write("(echo \"\")\r\n")
	out.write("(eval oper"+ str(i) +")\r\n")
	out.write("(eval cond"+ str(i) +")\r\n")
	out.write("(eval flag"+ str(i) +")\r\n")

	out.write("(eval rd"+ str(i) +")\r\n")
	out.write("(eval rn"+ str(i) +")\r\n")
	out.write("(eval ro"+ str(i) +")\r\n")

	out.write("(eval imm"+ str(i) +"_used)\r\n")
	out.write("(eval imm"+ str(i) +")\r\n")

	out.write("(eval barrel_op"+ str(i) +")\r\n")
	out.write("(eval barrel_num"+ str(i) +")\r\n")








































