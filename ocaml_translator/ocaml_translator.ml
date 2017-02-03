
open Z3

exception TestFailedException of string

let config = [("model", "true"); ("proof", "false")]
let context = Z3.mk_context config

let echo message = Printf.printf  message ^ "\n" 

let add x y = Arithmetic.mk_add context [x;y]
let intSort () = Arithmetic.Integer.mk_sort context
let intValue n = Expr.mk_numeral_int context n (intSort ())

let boolSort () = Boolean.mk_sort context

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
let bitVecRotateRight bv rotates = BitVector.mk_ext_rotate_right context bv rotates
let extract high low bv = BitVector.mk_extract context high low bv

let makeConstructor name symbol symbols options ints = Datatype.mk_constructor_s context name symbol symbols options ints
let makeDatatype name constructors = Datatype.mk_sort_s context name constructors
let recordSort name symbol symbols options ints = makeDatatype name [(makeConstructor name symbol symbols options ints)]
let arraySort index_sort store_sort = Z3Array.mk_sort context index_sort store_sort
let select arr index = Z3Array.mk_select context arr index

let enumSort name enums = Enumeration.mk_sort_s context name enums
let enumAt enum i = Enumeration.get_const enum i

let equals x y = Boolean.mk_eq context x y
let and_log clauses = Boolean.mk_and context clauses
let or_log clauses = Boolean.mk_or context clauses
let not_log x = Boolean.mk_not context x 
let ite cond t f = Boolean.mk_ite context cond t f

let symbol name = Symbol.mk_string context name
let freshConst name typ = Expr.mk_fresh_const context name typ
let const name typ = Expr.mk_const context (symbol name) typ

let solverAdd solver constraints = Solver.add solver constraints
let checkSat solver = Solver.check solver [] 
let getModel solver = Solver.get_model solver

let armConstraints num =
	
	Printf.printf "\nGenerating ARM Constraints\n\n";
	let int_sort = intSort () in
	let bool_sort = boolSort () in 
	let b1 = bitVecValue 1 1 in 
	let b0 = bitVecValue 0 1 in

	let opAND = bitVecValue 0 4 in
	let opEOR = bitVecValue 1 4 in
	let opSUB = bitVecValue 2 4 in
	let opRSB = bitVecValue 3 4 in
	let opADD = bitVecValue 4 4 in
	let opADC = bitVecValue 5 4 in
	let opSBC = bitVecValue 6 4 in
	let opRSC = bitVecValue 7 4 in
	let opTST = bitVecValue 8 4 in
	let opTEQ = bitVecValue 9 4 in
	let opCMP = bitVecValue 10 4 in
	let opCMN = bitVecValue 11 4 in
	let opORR = bitVecValue 12 4 in
	let opMOV = bitVecValue 13 4 in
	let opBIC = bitVecValue 14 4 in
	let opMVN = bitVecValue 15 4 in 

	let cEQ = bitVecValue 0 4 in
	let cNE = bitVecValue 1 4 in
	let cCS = bitVecValue 2 4 in
	let cCC = bitVecValue 3 4 in
	let cMI = bitVecValue 4 4 in
	let cPL = bitVecValue 5 4 in
	let cVS = bitVecValue 6 4 in
	let cVC = bitVecValue 7 4 in
	let cHI = bitVecValue 8 4 in
	let cLS = bitVecValue 9 4 in
	let cGE = bitVecValue 10 4 in
	let cLT = bitVecValue 11 4 in
	let cGT = bitVecValue 12 4 in
	let cLE = bitVecValue 13 4 in
	let cAL = bitVecValue 14 4 in

	let fN = bitVecValue 0 1 in
	let fS = bitVecValue 1 1 in

	let bLSL = bitVecValue 0 2 in
	let bLSR = bitVecValue 1 2 in
	let bASR = bitVecValue 2 2 in 
	let bROR = bitVecValue 3 2 in 

	let r0 =  bitVecValue 0 5 in 
	let r1 =  bitVecValue 1 5 in 
	let r2 =  bitVecValue 2 5 in 
	let r3 =  bitVecValue 3 5 in 
	let r4 =  bitVecValue 4 5 in 
	let r5 =  bitVecValue 5 5 in 
	let r6 =  bitVecValue 6 5 in 
	let r7 =  bitVecValue 7 5 in 
	let r8 =  bitVecValue 8 5 in 
	let r9 =  bitVecValue 9 5 in 
	let r10 = bitVecValue 10 5 in 
	let r11 = bitVecValue 11 5 in 
	let r12 = bitVecValue 12 5 in
	let sp =  bitVecValue 13 5 in 
	let lr =  bitVecValue 14 5 in 
	let pc =  bitVecValue 15 5 in  
	let cpsr = bitVecValue 16 5 in 

	let state = arraySort (bitVecSort 5) (bitVecSort 32) in
	let sequence = arraySort int_sort state in

	let instruction = (bitVecSort 32) in 
	let program = arraySort int_sort instruction in 

	let seq = const "seq" sequence in
	let prog = const "prog" program in 
	let conditionTrue pre cond = 
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

	let conditionFalse pre cond = (not_log (conditionTrue pre cond)) in

	let r0ToLrEqual pre post =
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

	let r0ToLrEqualExcept pre post rd =
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
	let generateContraints max =
		let rec generateContraints num max constraints = 
			if num = max then constraints 
			else
			let pre = select seq (intValue num) in
			let post = select seq (intValue (num + 1)) in

			let instr = select prog (intValue num) in 
			let oper = extract 24 21 instr in 
			let cond = extract 31 28 instr in 
			let flag_set = extract 20 20 instr in 

			let rd = bitVecConcat (bitVecValue 0 1) (extract 15 12 instr) in 
			let rn = bitVecConcat (bitVecValue 0 1) (extract 19 16 instr) in 

			let rm = bitVecConcat (bitVecValue 0 1) (extract 3 0 instr) in 
			let shift_mode = extract 4 4 instr in 
			let shift_type = extract 6 5 instr in 
			let rs = bitVecConcat (bitVecValue 0 1) (extract 11 8 instr) in 
			let shift_num = bitVecConcat (bitVecValue 0 27) (extract 11 7 instr) in 

			let imm_used = extract 25 25 instr in 
			let imm = bitVecConcat (bitVecValue 0 24) (extract 7 0 instr) in 
			let imm_shift = bitVecShiftLeft (bitVecConcat (bitVecValue 0 28) (extract 11 8 instr)) (bitVecValue 1 32) in 

			let rd_val = select post rd in
			let rn_val = select pre rn in
			let rm_val = select pre rm in 
			let rs_val = select pre rs in 

			let flex_val = 
				ite (equals imm_used b1)
					(bitVecRotateRight imm imm_shift) 
					(ite (equals shift_mode b0) 
						(ite 
							(equals shift_type bLSL)
							(bitVecShiftLeft rm_val shift_num)
							(ite 
								(equals shift_type bLSR)
								(bitVecLogicShiftRight rm_val shift_num)
								(ite 
									(equals shift_type bASR)
									(bitVecArithShiftRight rm_val shift_num)
									(bitVecRotateRight rm_val shift_num)
								)
							)
						)
						(ite 
							(equals shift_type bLSL)
							(bitVecShiftLeft rm_val rs_val)
							(ite 
								(equals shift_type bLSR)
								(bitVecLogicShiftRight rm_val rs_val)
								(ite 
									(equals shift_type bASR)
									(bitVecArithShiftRight rm_val rs_val)
									(bitVecRotateRight rm_val rs_val)
								)
							)
						) 
					) in 

			let condition_true = conditionTrue pre cond in
			let condition_false = conditionFalse pre cond in 
			let r0_to_lr_equal = r0ToLrEqual pre post in 
			let r0_to_lr_equal_except = r0ToLrEqualExcept pre post rd in 
			
			let constr = (and_log [
				(equals (bitVecAdd (select pre pc) (bitVecValue 4 32)) (select post pc));
				(or_log [
					(and_log [condition_false; r0_to_lr_equal]);
					(and_log [
						condition_true;
						(or_log [
							(and_log [
								r0_to_lr_equal;
								(or_log [(equals oper opCMP)]);
								(and_log [
									(or_log [
										(not_log (equals oper opCMP));
										(and_log [
											(or_log [
												(not_log (equals rn_val flex_val));
												(equals (extract 30 30 (select post cpsr)) b1)
											]);
											(or_log [
												(not_log (equals rn_val flex_val));
												(equals (extract 30 30 (select post cpsr)) b1)
											])
										])
									])
								])
							]);
							(and_log [
								r0_to_lr_equal_except;
								(or_log [
									(and_log [
										(or_log [(equals oper opMOV); (equals oper opMVN)]);
										(or_log [
											(not_log (equals oper opMOV));
											(equals rd_val flex_val)
										]);
										(or_log [
											(not_log (equals oper opMVN));
											(equals rd_val (bitVecNot flex_val) )
										])
									]);
									(and_log [
										(or_log [
											(equals oper opADD); (equals oper opSUB); (equals oper opAND)
										]);
										(or_log [
											(not_log (equals oper opADD));
											(equals rd_val (bitVecAdd (select pre rn) flex_val));
										]);
										(or_log [
											(not_log (equals oper opSUB));
											(equals rd_val (bitVecSub (select pre rn) flex_val));
										]);
										(or_log [
											(not_log (equals oper opAND));
											(equals rd_val (bitVecAnd (select pre rn) flex_val));
										])
									])
								]);
								(or_log [
									(and_log [
										(equals flag_set fN);
										(equals (select post cpsr) (select pre cpsr))
									]);
									(and_log [
										(equals flag_set fS);
										(or_log [
											(not_log (equals rd_val (bitVecValue 0 32)));
											(equals (extract 30 30 (select post cpsr)) b1)
										]);
										(or_log [
											(not_log (equals rd_val (bitVecValue 0 32)));
											(equals (extract 30 30 (select post cpsr)) b0)
										])
									])
								])
							]);
						
						])
					])
				])
				
			]) in 
			generateContraints (num + 1) max (constr::constraints)
		in
		generateContraints 0 max []
	in

	let constraints = generateContraints num in 
	
	let more = [
				(equals (select (select seq (intValue 0)) pc) (bitVecValue 4 32));
				(equals (select (select seq (intValue 1)) pc) (bitVecValue 8 32));
				(equals (select (select seq (intValue 0)) r0) (bitVecValue 0 32));
				(equals (select (select seq (intValue 0)) r1) (bitVecValue 2 32));
				(equals (select (select seq (intValue 1)) r0) (bitVecValue 2 32));
				(equals (select (select seq (intValue 1)) r1) (bitVecValue 2 32));
				] in
	let solver = Solver.mk_solver context None in
	solverAdd solver (constraints @ more); 
	let result = checkSat solver in
	if result != SATISFIABLE then 
		Printf.printf "UNSAT"
	else let model = getModel solver in
	match model with
	| None -> Printf.printf "NO MODEL"
	| Some (model) ->
		Printf.printf "Solver says: %s\n" (Solver.string_of_status result) ;
	  	Printf.printf "Model: \n%s\n" (Model.to_string model) ;

	Printf.printf "\nFinished\n";
	exit 0

let main = armConstraints 3

(*
	Use bitvectors for instructions V
	Record datatype X
	forall/exists quantifiers

	print readable output
*)






















	
