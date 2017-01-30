#!/usr/local/bin/python3

from z3 import *

print("Generating ARM Constraints")

Operation, (ADD, SUB, MOV, MVN, CMP, AND) = EnumSort("Operation", ("ADD", "SUB", "MOV", "MVN", "CMP", "AND"))
Condition, (EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, GE, LT, GT, LE, AL) = EnumSort("Condition", ("EQ", "NE", "CS", "CC", "MI", "PL", "VS", "VC", "HI", "LS", "GE", "LT", "GT", "LE", "AL"))
Flag, (N, S) = EnumSort("Flag", ("N", "S"))
BarrelOp, (LSL, LSR, ASR) = EnumSort("BarrelOp", ("LSL", "LSR", "ASR"))
Register, (R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, SP, LR, PC, CPSR) = EnumSort("Register", ("R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12", "SP", "LR", "PC", "CPSR")) 

State = ArraySort(Register, BitVecSort(32))
#Sequence = ArraySort(BitVecSort(3), State)

#seq = Const("seq", Sequence)

#transition = Function("transition", State, State, BoolSort())

cycles = int(sys.argv[1])
print("Cycles" , cycles)

states = []
opers = []
conds = []
flags = []
rds = []
rns = []
ros = []
imms = []
imm_useds = []
barrel_ops = []
barrel_nums = []
rd_vals = []
rn_vals = []
flex_vals = []
val_to_shifts = []


for i in range(0, cycles):
	states += [Const("state"+str(i), State)]
	opers += [Const("ops"+str(i), Operation)]
	conds += [Const("cond"+str(i), Condition)]
	flags += [Const("flag"+str(i), Flag)]
	rds += [Const("rd"+str(i), Register)]
	rns += [Const("rn"+str(i), Register)]
	ros += [Const("ro"+str(i), Register)]
	imms += [Const("imm"+str(i), BitVecSort(12))]
	imm_useds += [Const("imm_used"+str(i), BoolSort())]
	barrel_ops += [Const("barrel_op"+str(i), BarrelOp)]
	barrel_nums += [Const("barrel_num"+str(i), BitVecSort(32))]
	rd_vals += [Const("rd_val"+str(i), BitVecSort(32))]
	rn_vals += [Const("rn_val"+str(i), BitVecSort(32))]
	flex_vals += [Const("flex_val"+str(i), BitVecSort(32))]
	val_to_shifts += [Const("val_to_shift"+str(i), BitVecSort(32))]

solver = Solver()

for i in range(0, cycles - 1):
	solver.add(
		And(
			(Select(states[i], PC) + 4) == Select(states[i + 1], PC),
			Or(
				And(
					Not(
						Or(
							And(conds[i] == EQ, Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(1,1)),
							And(conds[i] == NE, Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(0,1)),
							And(conds[i] == CS, Extract(29, 29, Select(states[i], CPSR)) == BitVecVal(1,1)),
							And(conds[i] == CC, Extract(29, 29, Select(states[i], CPSR)) == BitVecVal(0,1)),
							And(conds[i] == MI, Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1)),
							And(conds[i] == PL, Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(0,1)),
							And(conds[i] == VS, Extract(28, 28, Select(states[i], CPSR)) == BitVecVal(1,1)),
							And(conds[i] == VC, Extract(28, 28, Select(states[i], CPSR)) == BitVecVal(0,1)),

							And(conds[i] == HI, And(Extract(29, 29, Select(states[i], CPSR)) == BitVecVal(1,1), Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(0,1))),
							And(conds[i] == LS, Or(Extract(29, 29, Select(states[i], CPSR)) == BitVecVal(0,1), Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(1,1))),
							And(conds[i] == GE, And(Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1), Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(1,1))),
							And(conds[i] == LT, Or(Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(0,1), Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(0,1))),
							And(conds[i] == GT, 
								And(
									Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(0,1),
									(Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1)) == (Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1))
								)),
							And(conds[i] == LE, 
								Or(
									Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(1,1),
									(Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1)) != (Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1))
								)),
							conds[i] == AL
						)
					),
					Select(states[i], R0) == Select(states[i + 1], R0),
					Select(states[i], R1) == Select(states[i + 1], R1),
					Select(states[i], R2) == Select(states[i + 1], R2),
					Select(states[i], R3) == Select(states[i + 1], R3),
					Select(states[i], R4) == Select(states[i + 1], R4),
					Select(states[i], R5) == Select(states[i + 1], R5),
					Select(states[i], R6) == Select(states[i + 1], R6),
					Select(states[i], R7) == Select(states[i + 1], R7),
					Select(states[i], R8) == Select(states[i + 1], R8),
					Select(states[i], R9) == Select(states[i + 1], R9),
					Select(states[i], R10) == Select(states[i + 1], R10),
					Select(states[i], R11) == Select(states[i + 1], R11),
					Select(states[i], R12) == Select(states[i + 1], R12),
					Select(states[i], SP) == Select(states[i + 1], SP),
					Select(states[i], LR) == Select(states[i + 1], LR),
					Select(states[i], CPSR) == Select(states[i + 1], CPSR)
				),
				And(
					If(imm_useds[i], val_to_shifts[i] == RotateRight(Concat(BitVecVal(0,24), Extract(7, 0, imms[i])), Concat(BitVecVal(0,28),Extract(11, 8, imms[i]))), val_to_shifts[i] == Select(states[i], ros[i])),
					And(
						Or(Not(barrel_ops[i] == LSL), flex_vals[i] == val_to_shifts[i] << barrel_nums[i]),
						Or(Not(barrel_ops[i] == LSR), flex_vals[i] == LShR(val_to_shifts[i], barrel_nums[i])),
						Or(Not(barrel_ops[i] == ASR), flex_vals[i] == val_to_shifts[i] >> barrel_nums[i])
					) ,
					rd_vals[i] == Select(states[i + 1], rds[i]),
					rn_vals[i] == Select(states[i], rns[i]),
					Or(
						And(conds[i] == EQ, Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(1,1)),
						And(conds[i] == NE, Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(0,1)),
						And(conds[i] == CS, Extract(29, 29, Select(states[i], CPSR)) == BitVecVal(1,1)),
						And(conds[i] == CC, Extract(29, 29, Select(states[i], CPSR)) == BitVecVal(0,1)),
						And(conds[i] == MI, Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1)),
						And(conds[i] == PL, Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(0,1)),
						And(conds[i] == VS, Extract(28, 28, Select(states[i], CPSR)) == BitVecVal(1,1)),
						And(conds[i] == VC, Extract(28, 28, Select(states[i], CPSR)) == BitVecVal(0,1)),

						And(conds[i] == HI, And(Extract(29, 29, Select(states[i], CPSR)) == BitVecVal(1,1), Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(0,1))),
						And(conds[i] == LS, Or(Extract(29, 29, Select(states[i], CPSR)) == BitVecVal(0,1), Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(1,1))),
						And(conds[i] == GE, And(Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1), Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(1,1))),
						And(conds[i] == LT, Or(Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(0,1), Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(0,1))),
						And(conds[i] == GT, 
							And(
								Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(0,1),
								(Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1)) == (Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1))
							)),
						And(conds[i] == LE, 
							Or(
								Extract(30, 30, Select(states[i], CPSR)) == BitVecVal(1,1),
								(Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1)) != (Extract(31, 31, Select(states[i], CPSR)) == BitVecVal(1,1))
							)),
						conds[i] == AL
					),
					Or(
						And(
							Or(opers[i] == CMP),

							Select(states[i], R0) == Select(states[i + 1], R0),
							Select(states[i], R1) == Select(states[i + 1], R1),
							Select(states[i], R2) == Select(states[i + 1], R2),
							Select(states[i], R3) == Select(states[i + 1], R3),
							Select(states[i], R4) == Select(states[i + 1], R4),
							Select(states[i], R5) == Select(states[i + 1], R5),
							Select(states[i], R6) == Select(states[i + 1], R6),
							Select(states[i], R7) == Select(states[i + 1], R7),
							Select(states[i], R8) == Select(states[i + 1], R8),
							Select(states[i], R9) == Select(states[i + 1], R9),
							Select(states[i], R10) == Select(states[i + 1], R10),
							Select(states[i], R11) == Select(states[i + 1], R11),
							Select(states[i], R12) == Select(states[i + 1], R12),
							Select(states[i], SP) == Select(states[i + 1], SP),
							Select(states[i], LR) == Select(states[i + 1], LR),
							Select(states[i], CPSR) == Select(states[i + 1], CPSR),

							Or(
								And(
									Not(opers[i] == CMP),
									If(rn_vals[i] == flex_vals[i],
										Extract(30, 30, Select(states[i + 1], CPSR)) == BitVecVal(1,1),
										Extract(30, 30, Select(states[i + 1], CPSR)) == BitVecVal(0,1),
									)
								),
							)
						),
						And(
							Or(rds[i] == R0, Select(states[i], R0) == Select(states[i + 1], R0)),
							Or(rds[i] == R1, Select(states[i], R1) == Select(states[i + 1], R1)),
							Or(rds[i] == R2, Select(states[i], R2) == Select(states[i + 1], R2)),
							Or(rds[i] == R3, Select(states[i], R3) == Select(states[i + 1], R3)),
							Or(rds[i] == R4, Select(states[i], R4) == Select(states[i + 1], R4)),
							Or(rds[i] == R5, Select(states[i], R5) == Select(states[i + 1], R5)),
							Or(rds[i] == R6, Select(states[i], R6) == Select(states[i + 1], R6)),
							Or(rds[i] == R7, Select(states[i], R7) == Select(states[i + 1], R7)),
							Or(rds[i] == R8, Select(states[i], R8) == Select(states[i + 1], R8)),
							Or(rds[i] == R9, Select(states[i], R9) == Select(states[i + 1], R9)),
							Or(rds[i] == R10, Select(states[i], R10) == Select(states[i + 1], R10)),
							Or(rds[i] == R11, Select(states[i], R11) == Select(states[i + 1], R11)),
							Or(rds[i] == R12, Select(states[i], R12) == Select(states[i + 1], R12)),
							Or(rds[i] == SP, Select(states[i], SP) == Select(states[i + 1], SP)),
							Or(rds[i] == LR, Select(states[i], LR) == Select(states[i + 1], LR)),
							Or(
								And(
									Or(opers[i] == MOV, opers[i] == MVN),
									Or(Not(opers[i] == MOV), rd_vals[i] == flex_vals[i]),
									Or(Not(opers[i] == MVN), rd_vals[i] == ~flex_vals[i])
								),
								And(
									Or(opers[i] == ADD, opers[i] == SUB, opers[i] == AND),
									Or(Not(opers[i] == ADD), rd_vals[i] == rn_vals[i] + flex_vals[i]),
									Or(Not(opers[i] == SUB), rd_vals[i] == rn_vals[i] - flex_vals[i]),
									Or(Not(opers[i] == AND), rd_vals[i] == rn_vals[i] & flex_vals[i]),
								)
							),
							Or(
								And(
									flags[i] == N, 
									Select(states[i], CPSR) == Select(states[i + 1], CPSR)
								),
								And(
									flags[i] == S,
									If(Select(states[i + 1], rds[i]) == BitVecVal(0, 32),
										Extract(30, 30, Select(states[i + 1], CPSR)) == BitVecVal(1,1),
										Extract(30, 30, Select(states[i + 1], CPSR)) == BitVecVal(0,1)
									)
								)
							)
						)
					)	
				)
			) 
		)
	)

reg = Const("reg", Register)

'''
x = BitVec("x", 3)
solver.add(
	ForAll(x, 
		Exists([])
		)
)
'''

print(solver.check())
if solver.check() == sat:
	print(solver.model())

'''
for i in range(0, cycles - 1):
	print("Instruction " + str(i))
	print(ops[i])
	print("")
'''





























