(* polyc -o sml_translator sml_translator.sml *)

fun spaceListR [] acc = acc
	|spaceListR (h::t) acc = spaceListR t (acc ^ " " ^ h)

fun spaceList li = spaceListR li ""

fun Echo str = "(echo \"" ^ str ^ "\")"

fun BitVecAdd x y = "(bvadd " ^ x ^ " " ^ y ^ ")"
fun BitVecSub x y = "(bvsub " ^ x ^ " " ^ y ^ ")"
fun BitVecAnd x y = "(bvand " ^ x ^ " " ^ y ^ ")"
fun BitVecNot x = "(bvnot " ^ x ^ ")"
fun BitVecConcat x y = "(concat " ^ x ^ " " ^ y ^ ")" 

fun Add x y = "(+ " ^ x ^ " " ^ y ^ ")"

fun BitVecValueR value size acc = 
	if size = 0 then ("#b" ^ acc) 
	else BitVecValueR (value div 2) (size - 1) ((Int.toString (value mod 2)) ^ acc)

fun BitVecValue value size = BitVecValueR value size ""

fun BitVecShiftLeft x y = "(bvshl " ^ x ^ " " ^ y ^ ")" 
fun BitVecArithShiftRight x y = "(bvashr " ^ x ^ " " ^ y ^ ")" 
fun BitVecLogicalShiftRight x y = "(bvlshr " ^ x ^ " " ^ y ^ ")" 
fun BitVecRotateRight x y = "((_ rotate_right " ^ y ^ ") " ^ x ^ ")" 

fun RecordSelect id record = "(" ^ id ^ " " ^ record ^ ")"
fun Select arr index = "(select " ^ arr ^ " " ^ index ^ ")"
fun Extract high low arr = "((_ extract " ^ high ^ " " ^ low ^ ") " ^ arr ^ ")"

fun BitVecSort size = "(_ BitVec " ^ size ^ ")"
fun ArraySort index_type var_type = "(Array " ^ index_type ^ " " ^ var_type ^ ")"
fun DefineSort name ty = "(define-sort " ^ name ^ " () " ^ ty ^ ")"
fun DataType name enums = "(declare-datatypes () ((" ^ name ^ " " ^ (spaceList enums) ^ ")))"

fun RecordR name [] terms = "(declare-datatypes () ((" ^ name ^ " (mk-" ^ name ^ " " ^ terms ^ "))))"
	|RecordR name ((id, ty)::t) terms = RecordR name t (terms ^ " (" ^ id ^ " " ^ ty ^ ")") 

fun Record name enums = RecordR name enums ""

fun And li = "(and \n" ^ (spaceList li) ^ "\n)"
fun Or li = "(or \n" ^ (spaceList li) ^ "\n)"
fun Not x = "(not " ^ x ^ ")"
fun Equals x y = "(= " ^ x ^ " " ^ y ^ ")"
fun If cond t f = "(ite " ^ cond ^ " " ^ t ^ " " ^ f ^ ")" 

fun Const name ty = "(declare-const " ^ name ^ " " ^ ty ^")"
fun Assert expr = "(assert " ^ expr ^ ")"

fun ForAllR [] terms body = "(forall (" ^ terms ^ ") " ^ body ^ ")"
	|ForAllR ((var, value)::t) terms body = ForAllR t (terms ^ " (" ^ var ^ " " ^ value ^ ")") body

fun ForAll li body = ForAllR li "" body

fun ExistsR [] terms body = "(exists (" ^ terms ^ ") " ^ body ^ ")"
	|ExistsR ((var, value)::t) terms body = ExistsR t (terms ^ " (" ^ var ^ " " ^ value ^ ")") body

fun Exists li body = ExistsR li "" body

fun LetR [] terms body = "(let (" ^ terms ^ ")" ^ body ^ ")"
	|LetR ((var, value)::t) terms body = LetR t (terms ^ " (" ^ var ^ " " ^ value ^ ")") body

fun Let li body = LetR li "" body

fun CheckSat () = "(check-sat)"
fun GetModel () = "(get-model)"

val ADD = "ADD"
val SUB = "SUB"
val MOV = "MOV"
val MVN = "MVN"
val CMP = "CMP"
val AND = "AND"

val EQ = "EQ"
val NE = "NE"
val CS = "CS"
val CC = "CC"
val MI = "MI"
val PL = "PL"
val VS = "VS"
val VC = "VC"
val HI = "HI"
val LS = "LS"
val GE = "GE"
val LT = "LT"
val GT = "GT"
val LE = "LE"
val AL = "AL"

val N = "N"
val S = "S"

val LSL = "LSL"
val LSR = "LSR"
val ASR = "ASR"

val R0 = "R0"
val R1 = "R1"
val R2 = "R2"
val R3 = "R3"
val R4 = "R4"
val R5 = "R5"
val R6 = "R6"
val R7 = "R7"
val R8 = "R8"
val R9 = "R9"
val R10 = "R10"
val R11 = "R11"
val R12 = "R12"
val SP = "SP"
val LR = "LR"
val PC = "PC"
val CPSR = "CPSR"

val condition_true = 
	(Or [
		(And [(Equals "cond" EQ), (Equals (Extract "30" "30" (Select "pre" CPSR)) "#b1") ]),
		(And [(Equals "cond" NE), (Equals (Extract "30" "30" (Select "pre" CPSR)) "#b0") ]),
		(And [(Equals "cond" CS), (Equals (Extract "29" "29" (Select "pre" CPSR)) "#b1") ]),
		(And [(Equals "cond" CC), (Equals (Extract "29" "29" (Select "pre" CPSR)) "#b0") ]),
		(And [(Equals "cond" MI), (Equals (Extract "31" "31" (Select "pre" CPSR)) "#b1") ]),
		(And [(Equals "cond" PL), (Equals (Extract "31" "31" (Select "pre" CPSR)) "#b0") ]),
		(And [(Equals "cond" VS), (Equals (Extract "28" "28" (Select "pre" CPSR)) "#b1") ]),
		(And [(Equals "cond" VC), (Equals (Extract "28" "28" (Select "pre" CPSR)) "#b0") ]),

		(And [
			(Equals "cond" GT),
			(And [
				(Equals (Extract "30" "30" (Select "pre" CPSR)) "#b0"),
				(Equals (Equals (Extract "31" "31" (Select "pre" CPSR)) "#b1") (Equals (Extract "28" "28" (Select "pre" CPSR)) "#b1"))
			])
		]),
		(And [
			(Equals "cond" LE),
			(Or [
				(Equals (Extract "30" "30" (Select "pre" CPSR)) "#b1"),
				(Not (Equals (Equals (Extract "31" "31" (Select "pre" CPSR)) "#b1") (Equals (Extract "28" "28" (Select "pre" CPSR)) "#b1")))
			])
		]),
		(Equals "cond" "AL")
	])

val condition_false = (Not condition_true)

val r0_to_lr_equal = 
	(And [
		(Equals (Select "pre" R0) (Select "post" R0)),
		(Equals (Select "pre" R1) (Select "post" R1)),
		(Equals (Select "pre" R2) (Select "post" R2)),
		(Equals (Select "pre" R3) (Select "post" R3)),
		(Equals (Select "pre" R4) (Select "post" R4)),
		(Equals (Select "pre" R5) (Select "post" R5)),
		(Equals (Select "pre" R6) (Select "post" R6)),
		(Equals (Select "pre" R7) (Select "post" R7)),
		(Equals (Select "pre" R8) (Select "post" R8)),
		(Equals (Select "pre" R9) (Select "post" R9)),
		(Equals (Select "pre" R10) (Select "post" R10)),
		(Equals (Select "pre" R11) (Select "post" R11)),
		(Equals (Select "pre" R12) (Select "post" R12)),
		(Equals (Select "pre" SP) (Select "post" SP)),
		(Equals (Select "pre" LR) (Select "post" LR))
	])

val r0_to_lr_equal_except =
	(And [
		(Or [(Equals (Select "pre" R0) (Select "post" R0)), (Equals "rd" R0)] ),
		(Or [(Equals (Select "pre" R1) (Select "post" R1)), (Equals "rd" R1)] ),
		(Or [(Equals (Select "pre" R2) (Select "post" R2)), (Equals "rd" R2)] ),
		(Or [(Equals (Select "pre" R3) (Select "post" R3)), (Equals "rd" R3)] ),
		(Or [(Equals (Select "pre" R4) (Select "post" R4)), (Equals "rd" R4)] ),
		(Or [(Equals (Select "pre" R5) (Select "post" R5)), (Equals "rd" R5)] ),
		(Or [(Equals (Select "pre" R6) (Select "post" R6)), (Equals "rd" R6)] ),
		(Or [(Equals (Select "pre" R7) (Select "post" R7)), (Equals "rd" R7)] ),
		(Or [(Equals (Select "pre" R8) (Select "post" R8)), (Equals "rd" R8)] ),
		(Or [(Equals (Select "pre" R9) (Select "post" R9)), (Equals "rd" R9)] ),
		(Or [(Equals (Select "pre" R10) (Select "post" R10)), (Equals "rd" R10)] ),
		(Or [(Equals (Select "pre" R11) (Select "post" R11)), (Equals "rd" R11)] ),
		(Or [(Equals (Select "pre" R12) (Select "post" R12)), (Equals "rd" R12)] ),
		(Or [(Equals (Select "pre" SP) (Select "post" SP)), (Equals "rd" SP)] ),
		(Or [(Equals (Select "pre" LR) (Select "post" LR)), (Equals "rd" LR)] )
	])


fun sequenceConstraintsR acc num max = 
	if num = max then acc
	else sequenceConstraintsR 
	(acc ^ (Assert 
		(Let
			[("num", Int.toString num)]
			(Let
				[
					("pre", Select "sequence" "num"),
					("post", Select "sequence" (Add "num" "1") ),
					("oper", (RecordSelect "operx" (Select "program" "num"))),
					("cond", (RecordSelect "condx" (Select "program" "num"))),
					("flag", (RecordSelect "flagx" (Select "program" "num"))),
					("rd", (RecordSelect "rdx" (Select "program" "num"))),
					("rn", (RecordSelect "rnx" (Select "program" "num"))),
					("ro", (RecordSelect "rox" (Select "program" "num"))),
					("imm", (RecordSelect "immx" (Select "program" "num"))),
					("imm_used", (RecordSelect "imm_usedx" (Select "program" "num"))),
					("barrel_op", (RecordSelect "barrel_opx" (Select "program" "num"))),
					("barrel_num", (RecordSelect "barrel_numx" (Select "program" "num")))
				]	
				(And[
					(Equals (BitVecAdd (Select "pre" "PC") (BitVecValue 4 32)) (Select "post" "PC")), "\r\n",
					(Or[
						(And [condition_false, r0_to_lr_equal]),
						(Let 
							[
								("flex_val", 
									(Let [("val_to_shift",
											(If "imm_used"
												(BitVecRotateRight (BitVecConcat (BitVecValue 0 24) (Extract "7" "0" "imm")) "0")
												(Select "pre" "ro")
											)
											)]
										(If (Equals "barrel_op" LSL)
											(BitVecShiftLeft "val_to_shift" "barrel_num")
											(If (Equals "barrel_op" LSR) 
												(BitVecLogicalShiftRight "val_to_shift" "barrel_num") 
												(BitVecArithShiftRight "val_to_shift" "barrel_num")
											)
										)
									)
								)
							]
							(And [
								condition_true,
								(Or [
									(And [
										r0_to_lr_equal,
										(Or [(Equals "oper" CMP)]),
										(And [
											(Or [
												(Not (Equals "oper" CMP)),
												(And[
													(Or [
														(Not (Equals (Select "pre" "rn") "flex_val")),
														(Equals (Extract "30" "30" (Select "post" CPSR)) "#b1")
													]),
													(Or [
														(Equals (Select "pre" "rn") "flex_val"),
														(Equals (Extract "30" "30" (Select "post" CPSR)) "#b0")
													])
												])
											])
										])
									]),
									(And [
										r0_to_lr_equal_except,
										(Or [
											(And [
												(Or [(Equals "oper" MOV), (Equals "oper" MVN)]),
												(Or [(Not (Equals "oper" MOV)), (Equals (Select "post" "rd") "flex_val")]),
												(Or [(Not (Equals "oper" MVN)), (Equals (Select "post" "rd") (BitVecNot "flex_val") )])
											]),
											(And [
												(Or [(Equals "oper" ADD), (Equals "oper" SUB), (Equals "oper" AND)]),
												(Or [(Not (Equals "oper" ADD)), (Equals (Select "post" "rd") (BitVecAdd (Select "pre" "rn") "flex_val") )]),
												(Or [(Not (Equals "oper" SUB)), (Equals (Select "post" "rd") (BitVecSub (Select "pre" "rn") "flex_val") )]),
												(Or [(Not (Equals "oper" AND)), (Equals (Select "post" "rd") (BitVecAnd (Select "pre" "rn") "flex_val") )])
											])
										]),
										(Or [
											(And [(Equals "flag" N), (Equals (Select "pre" CPSR) (Select "post" CPSR))]),
											(And [
												(Equals "flag" S),
												(Or [
													(Not (Equals (Select "post" "rd") (BitVecValue 0 32))),
													(Equals (Extract "30" "30" (Select "post" CPSR)) "#b1")
												]),
												(Or [
													(Equals (Select "post" "rd") (BitVecValue 0 32)),
													(Equals (Extract "30" "30" (Select "post" CPSR)) "#b0")
												])
											])
										])
									])
								])
							])
						)
					]), "\r\n"
				])
			) 
		)
	)) (num + 1) max

fun sequenceConstraints num_steps = sequenceConstraintsR "" 0 num_steps

fun generateArmConstraints num_steps = 
	(Echo "Generating ARM Constraints") ^ "\r\n" ^
	"(set-option :smt.mbqi true)" ^ "\r\n" ^

	DataType "Operation" [ADD, SUB, MOV, MVN, CMP, AND] ^ "\r\n" ^
	DataType "Condition" [EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, GE, LT, GT, LE, AL] ^ "\r\n" ^
	DataType "Flag" [N, S] ^ "\r\n" ^
	DataType "BarrelOp" [LSL, LSR, ASR] ^ "\r\n" ^
	DataType "Register" [R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, SP, LR, PC, CPSR] ^ "\r\n" ^
	
	DefineSort "State" (ArraySort "Register" (BitVecSort "32")) ^ "\r\n" ^
	Record "Instruction" [
		("operx", "Operation"),
		("condx", "Condition"),
		("flagx", "Flag"),
		("rdx", "Register"),
		("rnx", "Register"),
		("rox", "Register"),
		("immx", BitVecSort "12"),
		("imm_usedx", "Bool"),
		("barrel_opx", "BarrelOp"),
		("barrel_numx", BitVecSort "32")
	] ^ "\r\n" ^

	DefineSort "Sequence" (ArraySort "Int" "State") ^ "\r\n" ^
	DefineSort "Program" (ArraySort "Int" "Instruction") ^ "\r\n" ^

	Const "sequence" "Sequence" ^ "\r\n" ^
	Const "program" "Program" ^ "\r\n" ^

	(sequenceConstraints num_steps) ^ "\r\n" ^
	
	(Assert (Equals (Select (Select "sequence" "0") PC) (BitVecValue 1 32))) ^ "\r\n" ^
	(Assert (Equals (Select (Select "sequence" "2") PC) (BitVecValue 9 32))) ^ "\r\n" ^
	(CheckSat ()) ^ "\r\n" ^
	(GetModel ()) ^ "\r\n"

fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

fun main () = 
	let 
		val constraints = generateArmConstraints 8
	in
		writeFile "sml_gen.smt2" constraints
	end













