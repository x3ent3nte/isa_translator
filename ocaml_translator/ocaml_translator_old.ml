
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

type instruction = 
	{
		operx : Expr.expr;
		condx : Expr.expr;
		flagx : Expr.expr;
		rdx : Expr.expr;
		rnx : Expr.expr;
		rox : Expr.expr;
		immx : Expr.expr;
		imm_usedx : Expr.expr;
		barrel_opx : Expr.expr;
		barrel_numx : Expr.expr
	}

let armConstraints num =
	
	Printf.printf "Generating ARM Constraints\n";
	let int_sort = intSort () in
	let bool_sort = boolSort () in 
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

	let initialiseProgram num = 
		let rec initialiseProgram n acc =
			match n with
			|0 -> acc
			|_ -> initialiseProgram (n - 1) 
			({
				operx = (const ("oper_" ^ (string_of_int n)) operation);
				condx = (const ("cond_" ^ (string_of_int n)) condition);
				flagx = (const ("flag_" ^ (string_of_int n)) flag);
				rdx = (const ("rd_" ^ (string_of_int n)) register);
				rnx = (const ("rn_" ^ (string_of_int n)) register);
				rox = (const ("ro_" ^ (string_of_int n)) register);
				immx = (const ("imm_" ^ (string_of_int n)) (bitVecSort 12));
				imm_usedx = (const ("imm_used_" ^ (string_of_int n)) bool_sort);
				barrel_opx = (const ("barrel_op_" ^ (string_of_int n)) barrel_op);
				barrel_numx = (const ("barrel_num_" ^ (string_of_int n)) (bitVecSort 32))
			}::acc)
		in 
		initialiseProgram num [] in 
	(*
	let instruction = recordSort "Instruction" (symbol "Instruction") [
		(symbol "operx"); (symbol "condx"); (symbol "flagx"); (symbol "rdx"); (symbol "rnx"); (symbol "rox"); 
		(symbol "immx"); (symbol "imm_usedx"); (symbol "barrel_opx"); (symbol "barrel_numx");
		] [
			Some operation; Some condition; Some flag; Some register; Some register; Some register; 
			Some (bitVecSort 12); Some bool_sort; Some barrel_op; Some (bitVecSort 32)
		] [0;1;2;3;4;5;6;7;8;9] in
	let program = arraySort int_sort instruction in

	let seq = const "seq" sequence in
	let prog = const "prog" program in
	*)

	let seq = const "seq" sequence in
	let program = initialiseProgram num in
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
			(*
			let instr = select prog (intValue num) in
			let operd = List.nth (FuncDecl.get_parameters (List.nth (List.nth (Datatype.get_accessors instruction) 0) 0)) 0 in
			*)
			(*
			let (instruction, rest) = match program with
				| instr::tl -> (instr, tl) in
			*)
			let oper = const ("oper_" ^ (string_of_int num)) operation in 
			let cond = const ("cond_" ^ (string_of_int num)) condition in
			let flag = const ("flag_" ^ (string_of_int num)) flag in 
			let rd = const ("rd_" ^ (string_of_int num)) register in 
			let rn = const ("rn_" ^ (string_of_int num)) register in
			let ro = const ("ro_" ^ (string_of_int num)) register in 
			let imm = const ("imm_" ^ (string_of_int num)) (bitVecSort 12) in 
			let imm_used = const ("imm_used_" ^ (string_of_int num)) bool_sort in 
			let barrel_op = const ("barrel_op_" ^ (string_of_int num)) barrel_op in 
			let barrel_num = const ("barrel_num_" ^ (string_of_int num)) (bitVecSort 32) in

			let rd_val = select post rd in
			let rn_val = select pre rn in

			let val_to_shift = ite imm_used 
				(bitVecRotateRight (bitVecConcat (bitVecValue 0 24) (extract 7 0 imm)) (bitVecShiftLeft (bitVecConcat (bitVecValue 0 28) (extract 11 8 imm)) (bitVecValue 1 32)) ) 
									(select pre ro) in
			let flex_val = ite (equals barrel_op bLSL) (bitVecShiftLeft val_to_shift barrel_num) 
								(ite (equals barrel_op bLSR) (bitVecLogicShiftRight val_to_shift barrel_num) 
										(bitVecArithShiftRight val_to_shift barrel_num)) in

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
										(equals flag fN);
										(equals (select post cpsr) (select pre cpsr))
									]);
									(and_log [
										(equals flag fS);
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
	
	let more = [(equals (select (select seq (intValue 0)) pc) (bitVecValue 4 32));
				(equals (select (select seq (intValue 2)) pc) (bitVecValue 11 32));] in
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

	Printf.printf "Finished\n";
	exit 0

let main = armConstraints 7

(*
	Use bitvectors for instructions
	forall/exists qunatifiers
	Record datatype
*)






















	
