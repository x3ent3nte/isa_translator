
open Z3

let config = [("model", "true"); ("proof", "false")]
let context = Z3.mk_context config
let solver = Solver.mk_solver context None

let echo message = Printf.printf  message ^ "\n" 

let add x y = Arithmetic.mk_add context [x;y]
let intSort () = Arithmetic.Integer.mk_sort context
let intValue n = Symbol.mk_int n

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

let enumSort name enums = Enumeration.mk_sort_s context name enums
let enumAt enum i = Enumeration.get_const enum i

let equals x y = Boolean.mk_eq context x y
let and_log clauses = Boolean.mk_and context clauses
let or_log clauses = Boolean.mk_or context clauses
let not_log x = Boolean.mk_not context x 
let ite cond t f = Boolean.mk_ite context cond t f

let freshConst name typ = Expr.mk_fresh_const context name typ
let declareConst name typ = Expr.mk_const context name typ
let symbol name = Symbol.mk_string context name

let checkSat solver = Solver.check solver [] 
let getModel () = Solver.get_model solver

let _ =
	
	Printf.printf "Generating ARM Constraints\n";
	let int_sort = intSort () in
	let b1 = bitVecValue 1 1 in 
	let b0 = bitVecValue 0 1 in

	let operation = enumSort "Operation" ["ADD"; "SUB"; "MOV"; "MVN"; "CMP"; "AND"] in
	let condition = enumSort "Condition" [
		"EQ"; "NE"; "CS"; "CC"; "MI"; 
		"PL"; "VS"; "VC"; "HI"; "LS"; 
		"GE"; "LT"; "GT"; "LE"; "AL"] in 
	let flag = enumSort "Flag" ["N"; "S"] in
	let barrel_op = enumSort "BarrelOp" ["LSL"; "LSR"; "ASR"] in
	let register = enumSort "Register" [
		"R0"; "R1"; "R2"; "R3"; "R4"; "R5";
		"R6"; "R7"; "R8"; "R9"; "R10"; "R11"; 
		"R12"; "SP"; "LR"; "PC"; "CPSR"] in

	let opADD = enumAt operation 0 in
	let opSUB = enumAt operation 1 in
	let opMOV = enumAt operation 2 in
	let opMVN = enumAt operation 3 in
	let opCMP = enumAt operation 4 in
	let opAND = enumAt operation 5 in

	let cEQ = enumAt condition 0 in
	let cNE = enumAt condition 1 in
	let cCS = enumAt condition 2 in
	let cCC = enumAt condition 3 in
	let cMI = enumAt condition 4 in
	let cPL = enumAt condition 5 in
	let cVS = enumAt condition 6 in
	let cVC = enumAt condition 7 in
	let cHI = enumAt condition 8 in
	let cLS = enumAt condition 9 in
	let cGE = enumAt condition 10 in
	let cLT = enumAt condition 11 in
	let cGT = enumAt condition 12 in
	let cLE = enumAt condition 13 in
	let cAL = enumAt condition 14 in

	let fN = enumAt flag 0 in
	let fS = enumAt flag 1 in

	let bLSL = enumAt barrel_op 0 in
	let bLSR = enumAt barrel_op 1 in
	let bASR = enumAt barrel_op 2 in 

	let r0 =  enumAt register 0 in 
	let r1 =  enumAt register 1 in 
	let r2 =  enumAt register 2 in 
	let r3 =  enumAt register 3 in 
	let r4 =  enumAt register 4 in 
	let r5 =  enumAt register 5 in 
	let r6 =  enumAt register 6 in 
	let r7 =  enumAt register 7 in 
	let r8 =  enumAt register 8 in 
	let r9 =  enumAt register 9 in 
	let r10 =  enumAt register 10 in 
	let r11 =  enumAt register 11 in 
	let r12 =  enumAt register 12 in
	let sp =  enumAt register 13 in 
	let lr =  enumAt register 14 in 
	let pc =  enumAt register 15 in  
	let cpsr =  enumAt register 16 in 


	let state = arraySort register (bitVecSort 32) in
	let sequence = arraySort int_sort state in
	
	let condition_true pre cond = 
		(
			or_log [
				(and_log [(equals cond cEQ); (equals (extract 30 30 (select pre cpsr)) b1)]);
				(and_log [(equals cond cNE); (equals (extract 30 30 (select pre cpsr)) b0)]);
				(and_log [(equals cond cCS); (equals (extract 29 29 (select pre cpsr)) b1)]);
				(and_log [(equals cond cCC); (equals (extract 29 29 (select pre cpsr)) b0)]);
				(and_log [(equals cond cMI); (equals (extract 31 31 (select pre cpsr)) b1)]);
				(and_log [(equals cond cPL); (equals (extract 31 31 (select pre cpsr)) b0)]);
				(and_log [(equals cond cVS); (equals (extract 30 30 (select pre cpsr)) b1)]);
				(and_log [(equals cond cVC); (equals (extract 30 30 (select pre cpsr)) b0)]);
				(and_log [ 
					(equals cond cGT); (and_log [
						(equals (extract 30 30 (select pre cpsr)) b0);
						(equals (equals (extract 31 31 (select pre cpsr)) b1) (equals (extract 28 28 (select pre cpsr)) b1))
					])
				]);
				(and_log [ 
					(equals cond cLE); (and_log [
						(equals (extract 30 30 (select pre cpsr)) b1);
						(not_log (equals (equals (extract 31 31 (select pre cpsr)) b1) (equals (extract 28 28 (select pre cpsr)) b1)))
					])
				]);
				(equals cond cAL)
			]
		) in

	let condition_false pre cond = (not_log (condition_true pre cond)) in

	let r0_to_lr_equal pre post =
		(and_log [
			(equals (select pre r0) (select post r0));
			(equals (select pre r1) (select post r1));
			(equals (select pre r2) (select post r2));
			(equals (select pre r3) (select post r3));
			(equals (select pre r4) (select post r4));
			(equals (select pre r5) (select post r5));
			(equals (select pre r6) (select post r6));
			(equals (select pre r7) (select post r7));
			(equals (select pre r8) (select post r8));
			(equals (select pre r9) (select post r9));
			(equals (select pre r10) (select post r10));
			(equals (select pre r11) (select post r11));
			(equals (select pre r12) (select post r12));
			(equals (select pre sp) (select post sp));
			(equals (select pre lr) (select post lr));
		]) in

	let r0_to_lr_equal_except pre post rd =
		(and_log [
			(or_log [(equals (select pre r0) (select post r0)); (equals rd r0)]);
			(or_log [(equals (select pre r1) (select post r1)); (equals rd r1)]);
			(or_log [(equals (select pre r2) (select post r2)); (equals rd r2)]);
			(or_log [(equals (select pre r3) (select post r3)); (equals rd r3)]);
			(or_log [(equals (select pre r4) (select post r4)); (equals rd r4)]);
			(or_log [(equals (select pre r5) (select post r5)); (equals rd r5)]);
			(or_log [(equals (select pre r6) (select post r6)); (equals rd r6)]);
			(or_log [(equals (select pre r7) (select post r7)); (equals rd r7)]);
			(or_log [(equals (select pre r8) (select post r8)); (equals rd r8)]);
			(or_log [(equals (select pre r9) (select post r9)); (equals rd r9)]);
			(or_log [(equals (select pre r10) (select post r10)); (equals rd r10)]);
			(or_log [(equals (select pre r11) (select post r11)); (equals rd r11)]);
			(or_log [(equals (select pre r12) (select post r12)); (equals rd r12)]);
			(or_log [(equals (select pre sp) (select post sp)); (equals rd sp)]);
			(or_log [(equals (select pre lr) (select post lr)); (equals rd lr)]);
		]) in

	Printf.printf "Finished\n";
	exit 0






















	
