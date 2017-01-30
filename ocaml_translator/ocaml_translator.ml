
open Z3

let config = [("model", "true"); ("proof", "false")]
let context = Z3.mk_context config
let solver = Solver.mk_solver context None

let echo message = Printf.printf  message ^ "\n" 

let add x y = Arithmetic.mk_add context [x;y]
let intSort () = Arithmetic.Integer.mk_sort context

let bitVecValue value size = BitVector.mk_numeral context (string_of_int value) size
let bitVecSort size = BitVector.mk_sort context size
let bitVecAdd x y = BitVector.mk_add context x y
let bitVecSub x y = BitVector.mk_sub context x y
let bitVecAnd x y = BitVector.mk_and context x y
let bitVecNot x = BitVector.mk_not context x
let bitVecConcat x y = BitVector.mk_concat context x y
let bitVecShiftLeft bv shifts = BitVector.mk_shl context bv shifts
let bitVecArithShiftRight bv shifts = BitVector.mk_ashr context bv shifts
let bitVecLogicShiftRight bv shifts = BitVector.mk_lshr context bv shifts
let extract high low bv = BitVector.mk_extract context high low bv

let arraySort index_sort store_sort = Z3Array.mk_sort context index_sort store_sort
let select arr index = Z3Array.mk_select context arr index

let enumSort name enums = Enumeration.mk_sort context name enums

let equals x y = Boolean.mk_eq context x y
let and_log clauses = Boolean.mk_and context clauses
let or_log clauses = Boolean.mk_or context clauses
let not_log x y = Boolean.mk_not context x 
let ite cond t f = Boolean.mk_ite context cond t f

let declareConst name typ = Expr.mk_const context name typ
let symbol name = Symbol.mk_string context name

let checkSat solver = Solver.check solver [] 
let getModel () = Solver.get_model solver

let _ =
	let op_sym = symbol "Operation" in
	let op_add = symbol "ADD" in
	let op_sub = symbol "SUB" in
	let op_mov = symbol "MOV" in
	let op_mvn = symbol "MVN" in
	let op_cmp = symbol "CMP" in
	let op_and = symbol "AND" in

	let cond_sym = symbol "Condition" in
	let cond_eq = symbol "EQ" in
	let cond_ne = symbol "NE" in
	let cond_cs = symbol "CS" in
	let cond_cc = symbol "CC" in
	let cond_mi = symbol "MI" in
	let cond_pl = symbol "PL" in
	let cond_vs = symbol "VS" in
	let cond_vc = symbol "VC" in
	let cond_hi = symbol "HI" in
	let cond_ls = symbol "LS" in
	let cond_ge = symbol "GE" in
	let cond_lt = symbol "LT" in
	let cond_gt = symbol "GT" in
	let cond_le = symbol "LE" in
	let cond_al = symbol "AL" in

	let flag_sym = symbol "Flag" in
	let flag_N = symbol "N" in
	let flag_S = symbol "S" in

	let reg_sym = symbol "Register" in
	let reg_R0 = symbol "R0" in
	let reg_R1 = symbol "R1" in
	let reg_R2 = symbol "R2" in
	let reg_R3 = symbol "R3" in
	let reg_R4 = symbol "R4" in
	let reg_R5 = symbol "R5" in
	let reg_R6 = symbol "R6" in
	let reg_R7 = symbol "R7" in
	let reg_R8 = symbol "R8" in
	let reg_R9 = symbol "R9" in
	let reg_R10 = symbol "R10" in
	let reg_R11 = symbol "R11" in
	let reg_R12 = symbol "R12" in
	let reg_SP = symbol "SP" in
	let reg_LR = symbol "LR" in
	let reg_PC = symbol "PC" in
	let reg_CPSR = symbol "CPSR" in

	let barrel_op_sym = symbol "BarrelOp"in
	let barrel_op_LSL = symbol "LSL" in
	let barrel_op_LSR = symbol "LSR" in
	let barrel_op_ASR = symbol "ASR" in

	let pre = symbol "pre" in
	let post = symbol "post" in
	let oper = symbol "oper" in
	let cond = symbol "cond" in
	let flag = symbol "flag" in
	let rd = symbol "rd" in
	let rn = symbol "rn" in
	let ro = symbol "ro" in
	let imm = symbol "imm" in
	let imm_used = symbol "imm_used" in
	let barrel_op = symbol "barrel_op" in
	let barrel_num = symbol "barrel_num" in

	let int_sort = intSort () in

	let operation = enumSort op_sym [op_add; op_sub; op_mov; op_mvn; op_cmp; op_and] in
	let condition = enumSort cond_sym [
		cond_eq; cond_ne; cond_cs; cond_cc; 
		cond_mi; cond_pl; cond_vs; cond_vc; cond_hi; cond_ls; 
		cond_ge; cond_lt; cond_gt; cond_le; cond_al] in 
	let flag = enumSort flag_sym [flag_N; flag_S] in
	let barrel_op = enumSort barrel_op_sym [barrel_op_LSL; barrel_op_LSR; barrel_op_ASR] in
	let register = enumSort reg_sym [
		reg_R0; reg_R1; reg_R2; reg_R3; reg_R4; reg_R5; reg_R6;
		reg_R7; reg_R8; reg_R9; reg_R10; reg_R11; reg_R12; 
		reg_SP; reg_LR; reg_PC; reg_CPSR] in

	let state = arraySort register (bitVecSort 32) in
	let sequence = arraySort int_sort state in

	(*let condition_true = 
		(
			or_log [
				(and_log [(equals condition cond_eq); (equals (extract 30 30 (select pre reg_CPSR)) (bitVecValue 1 1))])
			]
		) in*)

	Printf.printf "lol\n";
	exit 0






















	
