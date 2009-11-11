open Util

type term = Var of char
	    | FOLfunction of char * term list;;

type connective = And | Or | Imp;;
type quantifier = Exists | Forall;;

(* a first order logic formula *)
type formula = Atom of char * term list
	       | Connective of connective * formula * formula
	       | Not of formula
	       | Quantifier of quantifier * char * formula
;;

(* number of arguments of a function or predicate *)
let rec arity (args : term list) =
  List.length args
;;

let rec term_to_str t =
  match t with
      Var(c) -> Char.escaped(c)
    | FOLfunction(c, []) -> Char.escaped(c)
    | FOLfunction(c, args) -> Char.escaped(c) ^ "(" ^ args_to_str(args) ^ ")"
and args_to_str args =
  match args with
      [] -> ""
    | x::[] -> term_to_str(x)
    | x::xs -> term_to_str(x) ^ "," ^ args_to_str(xs)
;;

let precedence f =
  match f with
      Atom(_) -> 5
    | Not(_) -> 4
    | Connective(And,_,_) -> 3
    | Connective(Or,_,_) -> 2
    | Connective(Imp,_,_) -> 1
    | Quantifier(_) -> 0
;;

let rec formula_to_str formula =
  let prec_formula = precedence formula in
  let paren f =
    let s_f = formula_to_str f in
      if prec_formula > precedence f then "(" ^ s_f ^ ")"  else s_f
  in
    match formula with
	Atom(c, []) -> Char.escaped(c)
      |	Atom(c, args) -> Char.escaped(c) ^ "(" ^ args_to_str(args) ^ ")"
      | Not(f) -> "~" ^ paren f
      | Connective(And, f1, f2) -> 
	  paren f1 ^ " ^ " ^ paren f2	  
      | Connective(Or, f1, f2) ->
	    paren f1 ^ " v " ^ paren f2
      | Connective(Imp, f1, f2) -> paren f1 ^ " => " ^ paren f2
      | Quantifier(Exists, c, f) -> "Exists(" ^ Char.escaped(c) ^ ")" ^ "(" ^ formula_to_str(f) ^ ")"
      | Quantifier(Forall, c, f) -> "Forall(" ^ Char.escaped(c) ^ ")" ^ "(" ^ formula_to_str(f) ^ ")"
;;

(* Set of variable simbols that appear in the arguments of a function or predicate *)
let rec argument_variables =
  function
    | [] -> CharSet.empty
    | Var(x)::xs -> CharSet.add x (argument_variables xs)
    | FOLfunction(f, args)::xs ->
	CharSet.union (argument_variables args) (argument_variables xs)
;;

(* set of free variable symbols(that are not in
   a quantifier scope *)
let rec free_variables =
  function Atom(c, args) -> argument_variables(args)
    | Connective(c, f1, f2)  ->
	CharSet.union (free_variables f1) (free_variables f2)	
    | Not(f) -> free_variables(f)
    | Quantifier(q, c, f) -> CharSet.remove c (free_variables f)
;;

(* set of bound variable symbols *)
let rec bound_variables =
  function
    | Connective(c, f1, f2) ->
	CharSet.union (bound_variables f1) (bound_variables f2)
    | Not(f) -> bound_variables f
    | Quantifier(q, c, f) -> CharSet.add c (bound_variables f)
    | _ -> CharSet.empty
;;

(* set of symbols(functions, vars and constants)
   used as arguments to a predicate *)
let rec argument_symbols =
    function
    | [] -> CharSet.empty
    | Var(x)::xs -> CharSet.add x (argument_symbols xs)
    | FOLfunction(f, args)::xs ->
	CharSet.add f (CharSet.union (argument_symbols args) (argument_symbols xs))
;;

(* set of function, variable and constant symbols
   used in a formula *)
let rec term_symbols  =
  function Atom(c, args) -> argument_symbols(args)
    | Connective(c, f1, f2) ->
	CharSet.union (term_symbols f1) (term_symbols f2)
    | Not(f) -> term_symbols(f)
    | Quantifier(q, c, f) -> CharSet.add c (term_symbols f)
;;

(* set of constant symbols *)
let rec constants formula =
  let rec cons_args =
    function
    | [] -> CharSet.empty
    | Var(x)::xs -> CharSet.empty
    | FOLfunction(f, [])::xs ->
	CharSet.add f (cons_args xs)
    | FOLfunction(f, args)::xs ->
	CharSet.union (cons_args args) (cons_args xs)
  in
    match formula with
	Not(f) -> constants(f)
      | Atom(c, args) -> cons_args args
      | Quantifier(_, _, f) -> constants f
      | Connective(_, f1, f2) ->
	  CharSet.union (constants f1) (constants f2)
;;

type substitution = {v : char; sv : term};;

(* applies a list of variable to term substitution
   to the predicate's arguments *)
let rec apply_substitution subs =
  function
    | [] -> []
    | x::xs ->	
	let s_x = (match x with
		       Var(c) ->
			 (try (List.find (fun t -> t.v = c) subs).sv
			  with _ -> x)
		     | FOLfunction(f, args) ->
			 FOLfunction (f, apply_substitution subs args))
	in
	  s_x::(apply_substitution subs xs)
;;	  
