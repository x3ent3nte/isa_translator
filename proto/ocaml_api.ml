(* ocamlfind ocamlopt -o ocaml_api -package Z3 -package core -linkpkg -thread ocaml_api.ml *)

(*open Core.Std*)

open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.FuncDecl
open Z3.Goal
open Z3.Tactic
open Z3.Tactic.ApplyResult
open Z3.Probe
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer
open Z3.Arithmetic.Real
open Z3.BitVector

exception TestFailedException of string
(*
let _ =
	Printf.printf "Generating ARM Constraints"

*)

let process_model (sol:solver) = 
	let model = (get_model sol) in
	match model with
	| None -> raise (TestFailedException "")
	| Some (model) ->
	  	Printf.printf "Model: \n%s\n" (Model.to_string model)
   
let test (ctx:context) =
	Printf.printf "Practice...\n\n";

	let fname = (mk_string ctx "h") in
	let x = (mk_string ctx "x") in
	let y = (mk_string ctx "y") in

	let boo = (Boolean.mk_sort ctx) in
	let domain = [boo; boo] in
	let func = (FuncDecl.mk_func_decl ctx fname domain boo) in

	let app = (mk_app ctx func [(Expr.mk_const ctx x boo); (Expr.mk_const ctx y boo)]) in
	let args2 = [(mk_fresh_const ctx "cp" boo)] in
	let domain2 = [boo] in
	let app2 = (mk_app ctx (mk_fresh_func_decl ctx "fp" domain2 boo) args2) in

	let trivial_eq = (mk_eq ctx app app) in
	let nontrivial_eq = (mk_eq ctx app app2) in
	let g = (mk_goal ctx true true false) in
	(Goal.add g [ trivial_eq ]);
	(Goal.add g [ nontrivial_eq ]);
	Printf.printf "%s\n" ("Goal: " ^ (Goal.to_string g));
	
	(
		let solver = (mk_solver ctx None) in
		(List.iter (fun a -> Solver.add solver [ a ])) (get_formulas g);
		if (check solver []) != SATISFIABLE then
			raise (TestFailedException "")
		else
			Printf.printf "\nValid.\n\n"
	)

let armv8 (ctx:context) = 
	Printf.printf "Creating constrants for ARM-V8\n\n";

	let reg = (BitVector.mk_sort ctx 32) in
	let flag = (BitVector.mk_sort ctx 1) in
	let cond = (BitVector.mk_sort ctx 4) in
	let state = (BitVector.mk_sort ctx 416) in

	let r0 = (BitVector.mk_sort ctx 32) in
	let r1 = (Expr.mk_const ctx (Symbol.mk_string ctx "R1") reg) in
	let r2 = (Expr.mk_const ctx (Symbol.mk_string ctx "R2") reg) in
	let r3 = (Expr.mk_const ctx (Symbol.mk_string ctx "R3") reg) in
	let r4 = (Expr.mk_const ctx (Symbol.mk_string ctx "R4") reg) in

	let add = (FuncDecl.mk_func_decl ctx (mk_string ctx "add") [cond; reg; reg; reg; state] state) in
	let add_app = (mk_app ctx add [Expr.mk_const ctx (mk_string ctx "state_before" state)]) in
	 
	
(*
	let registers = [
		(Symbol.mk_string ctx "R0");
		(Symbol.mk_string ctx "R1");
		(Symbol.mk_string ctx "R2");
		(Symbol.mk_string ctx "R3");
		(Symbol.mk_string ctx "R4");
		(Symbol.mk_string ctx "R5");
		(Symbol.mk_string ctx "R6");
		(Symbol.mk_string ctx "R7");
		(Symbol.mk_string ctx "R8");
		(Symbol.mk_string ctx "R9");
		(Symbol.mk_string ctx "R10");
		(Symbol.mk_string ctx "R11");
		(Symbol.mk_string ctx "R12");
		(Symbol.mk_string ctx "SP");
		(Symbol.mk_string ctx "LR");
		(Symbol.mk_string ctx "PC");
	] in

	let reg_constr = (Quantifier.mk_forall ctx reg registers) in*)
	let goal = (mk_goal ctx true true false) in
	let constr = (mk_eq ctx (Expr.mk_const ctx (Symbol.mk_string ctx "R0") (BitVector.mk_sort ctx 32)) (Expr.mk_const ctx (Symbol.mk_string ctx "R1") (BitVector.mk_sort ctx 32))) in
	(Goal.add goal [constr]);
	Printf.printf "%s\n" ("Goal: " ^ (Goal.to_string goal));

	(
		let solver = (mk_solver ctx None) in
		(List.iter (fun a -> (Solver.add solver [ a ])) (get_formulas goal)) ;
		if (check solver []) != SATISFIABLE then
			raise (TestFailedException "")
		else
			Printf.printf "\nValid.\n\n";
			process_model solver
	)

let rec factail n acc =
	match n with
	|1 -> acc
	|_ -> factail (n-1) (acc * n)

let factail (n) = factail n 1  


let rec factorial (n:int) =               
  	match n with
  	| 1 -> 1
  	| n -> n * factorial(n-1)

let rec fibonacci n x y =
 	match n with
 	| 0 -> x
  	| _ -> fibonacci (n-1) (y) (x + y)
                                     
let fibonacci n = fibonacci n 0 1                       
         
let _ =
 	Printf.printf "\nZ3 SMT solver version %s running \n\n" Version.to_string;
  	let cfg = [("model", "true"); ("proof", "false")] in
  	let ctx = (mk_context cfg) in
  	let is = (Symbol.mk_int ctx 42) in
  	let ss = (Symbol.mk_string ctx "mySymbol") in
  	let bs = (Boolean.mk_sort ctx) in
  	let ints = (Integer.mk_sort ctx) in
  	let rs = (Real.mk_sort ctx) in

  	Printf.printf "int symbol: %s\n" (Symbol.to_string is);
  	Printf.printf "string symbol: %s\n" (Symbol.to_string ss);
  	Printf.printf "bool sort: %s\n" (Sort.to_string bs);
  	Printf.printf "int sort: %s\n" (Sort.to_string ints);
  	Printf.printf "real sort: %s\n\n" (Sort.to_string rs);
  	
  	(*test ctx;*)
  	armv8 ctx;

  	Printf.printf "\nExiting\n\n";

  	(*Printf.printf "Factorial: %n\n" (factail 8);*)
        
  	exit 0






