
open Z3

let config = [("model", "true"); ("proof", "false")]
let context = Z3.mk_context config
let solver = Solver.mk_solver context None

let echo message = Printf.printf  message ^ "\n" 

let add x y = Arithmetic.mk_add context [x;y]

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
	let config = [("model", "true"); ("proof", "false")] in
	let context = Z3.mk_context config in
	let op_sym = Symbol.mk_string context "Operation" in
	let op_add = Symbol.mk_string context "ADD" in
	let op_sub = Symbol.mk_string context "SUB" in
	let op_mov = Symbol.mk_string context "MOV" in
	let op_mvn = Symbol.mk_string context "MVN" in
	let op_cmp = Symbol.mk_string context "CMP" in
	let op_and = Symbol.mk_string context "AND" in

	let cond_sym = Symbol.mk_string context "Condition" in
	let cond_eq = Symbol.mk_string context "EQ" in
	let cond_ne = Symbol.mk_string context "NE" in
	let cond_cs = Symbol.mk_string context "CS" in
	let cond_cc = Symbol.mk_string context "CC" in
	let cond_mi = Symbol.mk_string context "MI" in
	let cond_pl = Symbol.mk_string context "PL" in
	let cond_vs = Symbol.mk_string context "VS" in
	let cond_vc = Symbol.mk_string context "VC" in
	let cond_hi = Symbol.mk_string context "HI" in
	let cond_ls = Symbol.mk_string context "LS" in
	let cond_ge = Symbol.mk_string context "GE" in
	let cond_lt = Symbol.mk_string context "LT" in
	let cond_gt = Symbol.mk_string context "GT" in
	let cond_le = Symbol.mk_string context "LE" in
	let cond_al = Symbol.mk_string context "AL" in
	
	let operation = Enumeration.mk_sort context op_sym [op_add; op_sub; op_mov; op_mvn; op_cmp; op_and] in
	let condition = Enumeration.mk_sort context cond_sym [cond_eq; cond_ne; cond_cs; cond_cc; cond_mi; 
						cond_pl; cond_vs; cond_vc; cond_hi; cond_ls; cond_ge; cond_lt; cond_gt; cond_le; cond_al] in 
	Printf.printf "Crashes before here\n";
	exit 0






















	
