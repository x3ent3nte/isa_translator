
open Z3

let config = [("model", "true"); ("proof", "false")]
let context = Z3.mk_context config

(* A bunch of helper functions to make calling the Z3 API easier *)
let echo message = Printf.printf message ^ "\n" 

let add x y = Arithmetic.mk_add context [x;y]
let intSort () = Arithmetic.Integer.mk_sort context
let intValue n = Expr.mk_numeral_int context n (intSort ())

let boolSort () = Boolean.mk_sort context

let bitVecValue value size = BitVector.mk_numeral context (string_of_int value) size
let bitVecSort size = BitVector.mk_sort context size
let bitVecAdd x y = BitVector.mk_add context x y
let bitVecSub x y = BitVector.mk_sub context x y
let bitVecAnd x y = BitVector.mk_and context x y
let bitVecNand x y  = BitVector.mk_nand context x y
let bitVecOr x y = BitVector.mk_or context x y
let bitVecXor x y = BitVector.mk_xor context x y
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
let eval model expr flag = Model.eval model expr flag

(* These are some commonly used constants *)

let int_sort = intSort ()
let bool_sort = boolSort ()

(* Commonly used bit vector values *)

let b1 = bitVecValue 1 1
let b0 = bitVecValue 0 1
let zero_bv32 = bitVecValue 0 32
let four_bv32 = bitVecValue 4 32

(* The bit vector constants used to encode each op code. They have length 4 *)

let opAND = bitVecValue 0 4
let opEOR = bitVecValue 1 4
let opSUB = bitVecValue 2 4
let opRSB = bitVecValue 3 4
let opADD = bitVecValue 4 4
let opADC = bitVecValue 5 4
let opSBC = bitVecValue 6 4
let opRSC = bitVecValue 7 4
let opTST = bitVecValue 8 4
let opTEQ = bitVecValue 9 4
let opCMP = bitVecValue 10 4
let opCMN = bitVecValue 11 4
let opORR = bitVecValue 12 4
let opMOV = bitVecValue 13 4
let opBIC = bitVecValue 14 4
let opMVN = bitVecValue 15 4 

(* The bit vector contants used to encode each condition code. They have length 4 *)

let cEQ = bitVecValue 0 4
let cNE = bitVecValue 1 4
let cCS = bitVecValue 2 4
let cCC = bitVecValue 3 4
let cMI = bitVecValue 4 4
let cPL = bitVecValue 5 4
let cVS = bitVecValue 6 4
let cVC = bitVecValue 7 4
let cHI = bitVecValue 8 4
let cLS = bitVecValue 9 4
let cGE = bitVecValue 10 4
let cLT = bitVecValue 11 4
let cGT = bitVecValue 12 4
let cLE = bitVecValue 13 4
let cAL = bitVecValue 14 4
let cAL2 = bitVecValue 15 4

(* The bit vector constants used to encode the flag set symbol. They have length 1
fN means flags are not set
fS means flag are set *)

let fN = bitVecValue 0 1
let fS = bitVecValue 1 1

(* The bit vector constants used to encode the barrel operations. They have length 2 *)

let bLSL = bitVecValue 0 2
let bLSR = bitVecValue 1 2
let bASR = bitVecValue 2 2
let bROR = bitVecValue 3 2

(* The bit vector constants used to encode the registers + the cpsr
They have length 5. Arm registers are encoded by 4 bits but I used 5 bits for my encoding in order to make room for the cpsr *)

let r0 =  bitVecValue 0 5 
let r1 =  bitVecValue 1 5
let r2 =  bitVecValue 2 5
let r3 =  bitVecValue 3 5
let r4 =  bitVecValue 4 5
let r5 =  bitVecValue 5 5 
let r6 =  bitVecValue 6 5
let r7 =  bitVecValue 7 5
let r8 =  bitVecValue 8 5
let r9 =  bitVecValue 9 5
let r10 = bitVecValue 10 5
let r11 = bitVecValue 11 5
let r12 = bitVecValue 12 5
let sp =  bitVecValue 13 5
let lr =  bitVecValue 14 5
let pc =  bitVecValue 15 5 
let cpsr = bitVecValue 16 5 

(* A bunch of helper functions to help extract information from instructions and states*)

(* These return expressions which will extract the value of the NZCV flags *)
let flagN state = extract 31 31 (select state cpsr)
let flagZ state = extract 30 30 (select state cpsr)
let flagC state = extract 29 29 (select state cpsr)
let flagV state = extract 28 28 (select state cpsr)

(* These return a boolean expression which tests whether the NZCV are set to 1/0 *)
let isN1 state = equals (flagN state) b1
let isN0 state = equals (flagN state) b0
let isZ1 state = equals (flagZ state) b1
let isZ0 state = equals (flagZ state) b0
let isC1 state = equals (flagC state) b1
let isC0 state = equals (flagC state) b0
let isV1 state = equals (flagV state) b1
let isV0 state = equals (flagV state) b0

(* These return a boolean expression which tests whether a register value is postive/negative *)
let isRegisterValPositive bitvec = equals (extract 31 31 bitvec) b0
let isRegisterValNegative bitvec = equals (extract 31 31 bitvec) b1

(* These return expressions that extract informaton from an intruction i.e. bit vector 32, such as the op code, condition code etc *)
let getOper instr = extract 24 21 instr
let getCond instr = extract 31 28 instr 
let getFlagSet instr = extract 20 20 instr 

let getRd instr = bitVecConcat (bitVecValue 0 1) (extract 15 12 instr)
let getRn instr = bitVecConcat (bitVecValue 0 1) (extract 19 16 instr)

let getRm instr = bitVecConcat (bitVecValue 0 1) (extract 3 0 instr)
let getRs instr = bitVecConcat (bitVecValue 0 1) (extract 11 8 instr)

let getIsShiftedByRs instr = equals (extract 4 4 instr) b1
let getShiftType instr = extract 6 5 instr
let getShiftNum instr = bitVecConcat (bitVecValue 0 27) (extract 11 7 instr)

let getImmValue instr = bitVecConcat (bitVecValue 0 24) (extract 7 0 instr)
let getIsImmUsed instr = equals (extract 25 25 instr) b1
let getImmRotate instr = bitVecShiftLeft (bitVecConcat (bitVecValue 0 28) (extract 11 8 instr)) (bitVecValue 1 32)

(* state is an array sort indexed by bitvectors of size 5 (these encode the register names) 
and stores bitvectors of size 32 (these encode the register values) *)
let state = arraySort (bitVecSort 5) (bitVecSort 32)

(* sequence is an array sort indexed by integers (this represents time i.e. 0 indexes the first state, 1 indexes the next state etc)
It stores states, so represents the sequence of states that the program creates*)
let sequence = arraySort int_sort state

(* instruction is a bitvector of size 32 and encodes an arm instruction *)
let instruction = (bitVecSort 32)

(* program is an array sort indexed by bitvector 32 
This represents the address of the instruction and the value of a 
states program counter is the index of instruction that will be performed *)
let program = arraySort (bitVecSort 32) instruction


(* This function takes a state and returns an expression which is true if the NZCV flags meet the condition for execution *)
let conditionTrue state cond = 
	(or_log [
		(and_log [(equals cond cEQ); isZ1 state]);
		(and_log [(equals cond cNE); isZ0 state]);
		(and_log [(equals cond cCS); isC1 state]);
		(and_log [(equals cond cCC); isC0 state]);
		(and_log [(equals cond cMI); isN1 state]);
		(and_log [(equals cond cPL); isN0 state]);
		(and_log [(equals cond cVS); isV1 state]);
		(and_log [(equals cond cVC); isV0 state]);
		(and_log [
			(equals cond cHI); 
			(and_log [
				isC1 state;
				isZ0 state;
			])
		]);
		(and_log [
			(equals cond cLS); 
			(or_log [
				isC0 state;
				isZ1 state;
			])
		]);
		(and_log [
			(equals cond cGE); 
			(equals (isN1 state) (isV1 state))
		]);
		(and_log [
			(equals cond cLT); 
			(not_log (equals (isN1 state) (isV1 state)))
		]);
		(and_log [ 
			(equals cond cGT); 
			(and_log [
				isZ0 state;
				(equals (isN1 state) (isV1 state))
			])
		]);
		(and_log [ 
			(equals cond cLE); 
			(and_log [
				isZ1 state;
				(not_log (equals (isN1 state) (isV1 state)))
			])
		]);
		(equals cond cAL);
		(equals cond cAL2)
	]) 

(* This function takes a state and returns an expression which is false if the NZCV flags meet the condition for execution *)
let conditionFalse state cond = (not_log (conditionTrue state cond))

(* This function takes a pre state and post state and returns an expression which is true if the register values from R0 to LR
do not change between the states. Therfore the cpsr and program counter are allowed to change. This is used to constrain operations like CMP
which can set flags but no not write the result to a destination register *)
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
	])

(* This function takes a pre state and post state and a destination register and returns an expression which is true if the register values
from R0 to LR do not change between the states, unless it is the destination register. This is used to contrain operations like ADD and EOR
which can set flags and write the result to a destination register, i.e. all the other registers have to be unchanged *)

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
	])

(* This function takes a sequence constant, a program constant and the program length and returns an expression which 
constrains the state changes based on the instruction that connects them *)
let generateContraints seq prog max =
	let rec generateContraints seq prog num max constraints = 
		(* if num = max then the program length has been reach and the function returns the constraints*)
		if num = max then constraints 
		else

		(* We select the pre state and post state from the sequence array *)

		let pre = select seq (intValue num) in
		let post = select seq (intValue (num + 1)) in

		(* We select the instruction that links the pre and post state *)
		let instr = select prog (select pre pc) in 

		(* We then extract the necessary infomation ftom the instruction such as op code, condition code etc *)
		let oper = getOper instr in 
		let cond = getCond instr in 
		let flag_set = getFlagSet instr in 

		let rd = getRd instr in 
		let rn = getRn instr in 

		let rm = getRm instr in 
		let is_shifted_by_rs = getIsShiftedByRs instr in 
		let shift_type = getShiftType instr in 
		let rs = getRs instr in 
		let shift_num = getShiftNum instr in 

		let is_imm_used = getIsImmUsed instr in 
		let imm_val = getImmValue instr in 
		let imm_rotate_num = getImmRotate instr in 

		let rd_val = select post rd in
		let rn_val = select pre rn in
		let rm_val = select pre rm in 
		let rs_val = select pre rs in 

		(* flex_val is the value that Operand2 will evaluate to
		This dependes on whether an immediate value was used and the rotation applied to the immediate value, 
		or whether a register is used for Operand2, whether this register will be shifted by the value in another register, 
		or an immediate value, and the type of shift applied e.g. LSR *)
		let flex_val = 
			ite (is_imm_used)
				(bitVecRotateRight imm_val imm_rotate_num) 
				(ite is_shifted_by_rs 
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
				) in 
		(* call these functions to get the expressions which constrain what it means for the condition for execution to be true
		and the constraints on which registers must not change*)
		let condition_true = conditionTrue pre cond in
		let condition_false = conditionFalse pre cond in 
		let r0_to_lr_equal = r0ToLrEqual pre post in 
		let r0_to_lr_equal_except = r0ToLrEqualExcept pre post rd in 
		
		(* constr is the constraint that connects the pre and post states *)
		let constr = (and_log [
			(* assert that the program counter must increment by 4 *)
			(equals (bitVecAdd (select pre pc) four_bv32) (select post pc));

			(* There are two possibilities, either the condition is false, so the instruction is not executed,
			or the condition is true, so the instruction is executed.
			This OR clause has two AND clauses, and these represent these two possibilities *)
			(or_log [

				(* If the condition for execution is false, then the registers and cpsr must be equal *)
				(and_log [condition_false; r0_to_lr_equal; (equals (select pre cpsr) (select post cpsr))]);

				(* Else, the condition for execution must be true *)
				(and_log [
					condition_true;

					(* This OR clause contains two AND clauses.
					The first AND clause constrains the comparision operations. i.e. operation that don't write to registers 
					The second AND clause constrains operations that write to a destination register such as EOR, ADD, SUB*)
					(or_log [

						(* Constraints for Comparison operations*)
						(and_log [

							(* comparison operations do not change registers *)
							r0_to_lr_equal;

							(*Theye are cmp, cmn, tst ot teq*)
							(or_log [
								(equals oper opCMP);
								(equals oper opCMN);
								(equals oper opTST);
								(equals oper opTEQ);
							]);
							(*This AND clause constrains the effect that these operations have on the state*)
							(and_log [
								(* For example this OR clause says that if the operation is CMP then that implies various contraints
								on when flags are set
								 *)
								(or_log [
									(not_log (equals oper opCMP));
									(* so if the operation is CMP then this AND clause must be satisfied *)
									(and_log [
										(and_log [
											(* if the rn_val = the flex_val i.e. rn_val - flex_val = 0
											then the Z flag is 1 *)
											(or_log [
												(not_log (equals rn_val flex_val));
												(isZ1 post)
											]);
											(* if the rn_val != flex_val i.e. rn_val - flex_val != 0
											then the Z flag is 0 *)
											(or_log [
												(equals rn_val flex_val);
												(isZ0 post)
											])
										]);
										(* the AND clause constrains the setting of the N flag *)
										(and_log [
											(or_log [
												(not_log (isRegisterValNegative (bitVecSub rn_val flex_val)));
												(isN1 post)
											]);
											(or_log [
												(isRegisterValNegative (bitVecSub rn_val flex_val));
												(isN0 post)
											])
										]);
									])
								]);
								(or_log [
									(not_log (equals oper opCMN));
									(and_log [
										(and_log [
											(or_log [
												(not_log (equals (bitVecAdd rn_val flex_val) zero_bv32));
												(isZ1 post)
											]);
											(or_log [
												(equals (bitVecAdd rn_val flex_val) zero_bv32);
												(isZ0 post)
											])
										]);
										(and_log [
											(or_log [
												(not_log (isRegisterValNegative (bitVecAdd rn_val flex_val)));
												(isN1 post)
											]);
											(or_log [
												(isRegisterValNegative (bitVecAdd rn_val flex_val));
												(isN0 post)
											])
										]);
									])
								]);
								(or_log [
									(not_log (equals oper opTST));
									(and_log [
										(and_log [
											(or_log [
												(not_log (equals (bitVecAnd rn_val flex_val) zero_bv32));
												(isZ1 post)
											]);
											(or_log [
												(equals (bitVecAnd rn_val flex_val) zero_bv32);
												(isZ0 post)
											])
										]);
										(and_log [
											(or_log [
												(not_log (isRegisterValNegative (bitVecAnd rn_val flex_val)));
												(isN1 post)
											]);
											(or_log [
												(isRegisterValNegative (bitVecAnd rn_val flex_val));
												(isN0 post)
											])
										]);
									])
								]);
								(or_log [
									(not_log (equals oper opTEQ));
									(and_log [
										(and_log [
											(or_log [
												(not_log (equals (bitVecXor rn_val flex_val) zero_bv32));
												(isZ1 post)
											]);
											(or_log [
												(equals (bitVecXor rn_val flex_val) zero_bv32);
												(isZ0 post)
											])
										]);
										(and_log [
											(or_log [
												(not_log (isRegisterValNegative (bitVecXor rn_val flex_val)));
												(isN1 post)
											]);
											(or_log [
												(isRegisterValNegative (bitVecXor rn_val flex_val));
												(isN0 post)
											])
										]);
									])
								])

							])
						]);

						(* Constraints for operations that write to a destination register *)
						(and_log [
							(* include constraint that all register from r0 to lr except the destination register cannot change *)
							r0_to_lr_equal_except;

							(* This OR clause contains two AND clauses
							The first constrains operations MOV and MVN which take two arguments (i.e. no RN)
							The second constrains operations like ADD and SUB which take three arguments (RD, RN and Operand 2) *)
							(or_log [

								(* Constraints for Operations that take two arguments *)
								(and_log [
									(or_log [
										(equals oper opMOV); 
										(equals oper opMVN)
									]);
									(or_log [
										(not_log (equals oper opMOV));
										(equals rd_val flex_val)
									]);
									(or_log [
										(not_log (equals oper opMVN));
										(equals rd_val (bitVecNot flex_val) )
									])
								]);

								(* Constraints for Operations that take three arguments*)
								(and_log [
									(or_log [
										(equals oper opADD); 
										(equals oper opSUB);
										(equals oper opRSB); 
										(equals oper opAND);
										(equals oper opORR);
										(equals oper opEOR);
										(equals oper opBIC);
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
										(not_log (equals oper opRSB));
										(equals rd_val (bitVecSub flex_val (select pre rn)));
									]);
									(or_log [
										(not_log (equals oper opAND));
										(equals rd_val (bitVecAnd (select pre rn) flex_val));
									]);
									(or_log [
										(not_log (equals oper opORR));
										(equals rd_val (bitVecOr (select pre rn) flex_val));
									]);
									(or_log [
										(not_log (equals oper opEOR));
										(equals rd_val (bitVecXor (select pre rn) flex_val));
									]);
									(or_log [
										(not_log (equals oper opBIC));
										(equals rd_val (bitVecNand (select pre rn) flex_val));
									]);
								])
							]);

							(* This OR clauses constrains the effect of flags being set or not *)
							(or_log [

								(* If the flags are not set then the cpsr cannot change *)
								(and_log [
									(equals flag_set fN);
									(equals (select post cpsr) (select pre cpsr))
								]);
								(* If the flags are to be set then changes can happen *)
								(and_log [
									(equals flag_set fS);

									(* If the value in rd is zero then Z flag is 1 else it is 0 *)
									(and_log [
										(or_log [
											(not_log (equals rd_val zero_bv32));
											isZ1 post
										]);
										(or_log [
											(equals rd_val zero_bv32);
											isZ0 post
										])
									]);

									(* If the value in rd is negative (i.e. leading bit is 1) then N flag is 1 else it is 0 *)
									(and_log [
										(or_log [
											(not_log (isRegisterValNegative rd_val));
											isN1 post;
										]);
										(or_log [
											isRegisterValNegative rd_val;
											isN0 post;
										])
									]);
								])
							])
						]);
					])
				])
			])
			
		]) in 

		(* the constraint generated for this number is prepended to the list of constraints
		and the function is recursively called for the next number *)
		generateContraints seq prog (num + 1) max (constr::constraints)
	in
	generateContraints seq prog 0 max []

(* The printProgram function is rather long and kludgy and is used to print 
an assembly style representation of each instruction in the program *)
let printProgram model prog num = 
	let rec printProgram model prog num max =
	if num < max then 

		let matchBooleanEval opt =
			match opt with
			| None -> false
			| Some(opt) -> Boolean.is_true opt in

		let getOperStr instr =
			let bits = getOper instr in 
			if matchBooleanEval (eval model (equals bits opAND) true) then "AND"
			else if matchBooleanEval (eval model (equals bits opEOR) true) then "EOR"
			else if matchBooleanEval (eval model (equals bits opSUB) true) then "SUB"
			else if matchBooleanEval (eval model (equals bits opRSB) true) then "RSB"
			else if matchBooleanEval (eval model (equals bits opADD) true) then "ADD"
			else if matchBooleanEval (eval model (equals bits opADC) true) then "ADC"
			else if matchBooleanEval (eval model (equals bits opSBC) true) then "SBC"
			else if matchBooleanEval (eval model (equals bits opRSC) true) then "RSC"
			else if matchBooleanEval (eval model (equals bits opTST) true) then "TST"
			else if matchBooleanEval (eval model (equals bits opTEQ) true) then "TEQ"
			else if matchBooleanEval (eval model (equals bits opCMP) true) then "CMP"
			else if matchBooleanEval (eval model (equals bits opCMN) true) then "CMN"
			else if matchBooleanEval (eval model (equals bits opORR) true) then "ORR"
			else if matchBooleanEval (eval model (equals bits opMOV) true) then "MOV"
			else if matchBooleanEval (eval model (equals bits opBIC) true) then "BIC"
			else if matchBooleanEval (eval model (equals bits opMVN) true) then "MVN"
			else "No Oper!" in 

		let getCondStr instr =
			let bits = getCond instr in 
			if matchBooleanEval (eval model (equals bits cEQ) true) then "EQ"
			else if matchBooleanEval (eval model (equals bits cNE) true) then "NE"
			else if matchBooleanEval (eval model (equals bits cCS) true) then "CS"
			else if matchBooleanEval (eval model (equals bits cCC) true) then "CC"
			else if matchBooleanEval (eval model (equals bits cMI) true) then "MI"
			else if matchBooleanEval (eval model (equals bits cPL) true) then "PL"
			else if matchBooleanEval (eval model (equals bits cVS) true) then "VS"
			else if matchBooleanEval (eval model (equals bits cVC) true) then "VC"
			else if matchBooleanEval (eval model (equals bits cHI) true) then "HI"
			else if matchBooleanEval (eval model (equals bits cLS) true) then "LS"
			else if matchBooleanEval (eval model (equals bits cGE) true) then "GE"
			else if matchBooleanEval (eval model (equals bits cLT) true) then "LT"
			else if matchBooleanEval (eval model (equals bits cGT) true) then "GT"
			else if matchBooleanEval (eval model (equals bits cLE) true) then "LE"
			else if matchBooleanEval (eval model (equals bits cAL) true) then "AL"
			else if matchBooleanEval (eval model (equals bits cAL2) true) then "AL"
			else "No Cond!" in 

		let getFlagSetStr instr =
			let bits = getFlagSet instr in 
			if matchBooleanEval (eval model (equals bits fN) true) then " "
			else if matchBooleanEval (eval model (equals bits fS) true) then "S"
			else "No Flag_set!" in 

		let getImmUsed instr = 
			let bits_equal_b1 = getIsImmUsed instr in 
			matchBooleanEval (eval model bits_equal_b1 true) in 

		let getRegisterStr bits =
			if matchBooleanEval (eval model (equals bits r0) true) then "R0 "
			else if matchBooleanEval (eval model (equals bits r1) true) then "R1 "
			else if matchBooleanEval (eval model (equals bits r2) true) then "R2 "
			else if matchBooleanEval (eval model (equals bits r3) true) then "R3 "
			else if matchBooleanEval (eval model (equals bits r4) true) then "R4 "
			else if matchBooleanEval (eval model (equals bits r5) true) then "R5 "
			else if matchBooleanEval (eval model (equals bits r6) true) then "R6 "
			else if matchBooleanEval (eval model (equals bits r7) true) then "R7 "
			else if matchBooleanEval (eval model (equals bits r8) true) then "R8 "
			else if matchBooleanEval (eval model (equals bits r9) true) then "R9 "
			else if matchBooleanEval (eval model (equals bits r10) true) then "R10"
			else if matchBooleanEval (eval model (equals bits r11) true) then "R11"
			else if matchBooleanEval (eval model (equals bits r12) true) then "R12"
			else if matchBooleanEval (eval model (equals bits sp) true) then "SP "
			else if matchBooleanEval (eval model (equals bits lr) true) then "LR "
			else if matchBooleanEval (eval model (equals bits pc) true) then "PC "
			else "No Register!" in 

		let getRdStr instr =
			getRegisterStr (getRd instr) in 

		let getRnStr instr =
			getRegisterStr (getRn instr) in 

		let getRmStr instr = 
			getRegisterStr (getRm instr) in 

		let getRsStr instr = 
			getRegisterStr (getRs instr) in 

		let getShiftTypeStr instr =
			let bits = getShiftType instr in 
			if matchBooleanEval (eval model (equals bits bLSL) true) then "LSL" 
			else if matchBooleanEval (eval model (equals bits bLSR) true) then "LSR"
			else if matchBooleanEval (eval model (equals bits bASR) true) then "ASR"
			else if matchBooleanEval (eval model (equals bits bROR) true) then "ROR"
			else "No Barrel Shift!" in 

		let isShiftedByRs instr =
			let bits_equal_b1 = getIsShiftedByRs instr in 
			matchBooleanEval (eval model bits_equal_b1 true) in 

		let getBitVecStr bits =
			let opt = eval model bits true in 
			match opt with
			| None -> "No Bit Vector!"
			| Some(bitvec) -> BitVector.numeral_to_string bitvec in  

		let getShiftAmountStr instr =
			let bits = getShiftNum instr in 
			getBitVecStr bits in 

		let getImmValueStr instr = 
			let bits = getImmValue instr in 
			getBitVecStr bits in 

		let getImmRotateValueStr instr = 
			let bits = getImmRotate instr in 
			getBitVecStr bits in 

		let instr = (select prog (bitVecValue (num * 4) 32)) in

		let imm_used_bool = getImmUsed instr in 
		let shift_by_register_bool = isShiftedByRs instr in	

		let oper_str = getOperStr instr in
		let cond_str = getCondStr instr in 
		let flag_set_str = getFlagSetStr instr in 

		let rd_str = getRdStr instr in 
		let rn_str = getRnStr instr in

		let shift_type_str = getShiftTypeStr instr in 

		let rm_str = getRmStr instr in  
		let rs_str = getRsStr instr in
		let shift_amount = getShiftAmountStr instr in 

		let imm_str = getImmValueStr instr in 
		let imm_rotate_str = getImmRotateValueStr instr in 

		if imm_used_bool then 
			Printf.printf "%s %s %s %s %s %s %s \n" oper_str cond_str flag_set_str rd_str rn_str imm_str imm_rotate_str
		else
		if shift_by_register_bool then
			Printf.printf "%s %s %s %s %s %s %s %s \n" oper_str cond_str flag_set_str rd_str rn_str rm_str shift_type_str rs_str
			else
			Printf.printf "%s %s %s %s %s %s %s %s \n" oper_str cond_str flag_set_str rd_str rn_str rm_str shift_type_str shift_amount;
			
		printProgram model prog (num + 1) max in 
	printProgram model prog 0 num



(* armConstraints take an integer, which is the number of instructions in the program.
	It creates a sequence constant and a program constant and calls the function which creates constraints
	for these.
	Then a solver is created, the constraints are added to the solver, and the solver checks the satisfiability
	If it is unsat then UNSAT this is printed to the screen.
	If it is sat then the generated program is printed to the screen *)
let armConstraints num =
	Printf.printf "\nGenerating ARM Constraints\n\n"; 

	let seq = const "seq" sequence in
	let prog = const "prog" program in 

	let constraints = generateContraints seq prog num in 
	
	(* more_constraints is used to add addition constraints for testing purposes *)
	let more_constraints = [
		(equals (select (select seq (intValue 0)) pc) (bitVecValue 0 32));
		(equals (select (select seq (intValue 1)) pc) (bitVecValue 4 32));
		(equals (select (select seq (intValue 10)) pc) (bitVecValue 44 32));

		(equals (select (select seq (intValue 0)) r0) (bitVecValue 0 32));
		(equals (select (select seq (intValue 0)) r1) (bitVecValue 2 32));
		(equals (select (select seq (intValue 1)) r0) (bitVecValue 0 32));
		(equals (select (select seq (intValue 1)) r1) (bitVecValue 2 32));

		(equals (getOper (select prog (bitVecValue 0 32))) opTEQ);
		(equals (getCond (select prog (bitVecValue 0 32))) cEQ);
		(equals (getOper (select prog (bitVecValue 4 32))) opMOV);
		(equals (getCond (select prog (bitVecValue 4 32))) cNE);

		(isN1 (select seq (intValue 0)));
		(isN0 (select seq (intValue 1)));
		] in

	let solver = Solver.mk_solver context None in
	solverAdd solver (constraints @ more_constraints); 
	let result = checkSat solver in

	if result != SATISFIABLE then 
		Printf.printf "UNSAT\n\nFinished"
	else 
	let model = getModel solver in
	match model with
	| None -> Printf.printf "NO MODEL"
	| Some (model) ->
		Printf.printf "Solver says: %s\n" (Solver.string_of_status result) ;
	  	(* Printf.printf "Model: \n%s\n\n" (Model.to_string model); *)
		Printf.printf "\nGenerated Program:\n\n";

		printProgram model prog num;
		Printf.printf "\nFinished\n"


(* this line makes the program do stuff *)
let main = armConstraints 7

(*
	TODO

	Use bitvectors for instructions V
	Use Record datatype X
	print readable output V

	forall/exists quantifiers
	overflow/carry flag setting
	print out only rn and op2 for 2 register operations

	Use bitvec 32 for instruction indexing to enable branching V
*)








