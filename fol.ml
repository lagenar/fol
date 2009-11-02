open Util

(* non-empty argument list of a function or predicate *)
type 'a arguments = Arg of 'a | Arguments of 'a * 'a arguments;;

(* a logical term is either(variable or constant) an atomic term or a function *)
type term = Var of char
	    | Constant of char
	    | FOLfunction of char * term arguments;;

type connective = And | Or | Imp;;
type quantifier = Exists | Forall;;

(* a first order logic formula *)
type formula = Atom of char * term arguments
	       | Connective of connective * formula * formula
	       | Not of formula
	       | Quantifier of quantifier * char * formula
;;

(* number of arguments of a function or predicate *)
let rec arity args =
  match args with
      Arg(_) -> 1
    | Arguments(_, tl) -> 1 + arity(tl)
;;

let rec term_to_str t =
  match t with
      Var(c) -> Char.escaped(c)
    | Constant(c) -> Char.escaped(c)
    | FOLfunction(c, args) -> Char.escaped(c) ^ "(" ^ args_to_str(args) ^ ")"
and args_to_str args =
  match args with
      Arg(t) -> term_to_str(t)
    | Arguments(t, rest) -> term_to_str(t) ^ "," ^ args_to_str(rest);;

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
	Atom(c, args) -> Char.escaped(c) ^ "(" ^ args_to_str(args) ^ ")"
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
  function Arg(t) ->
    (match t with
	 Var(c) -> CharSet.add c CharSet.empty
       | FOLfunction(_, args) -> argument_variables args
       | _ -> CharSet.empty)
    | Arguments(Var(c), rest) -> CharSet.add c (argument_variables rest)
    | Arguments(Constant(c), rest) -> argument_variables rest
    | Arguments(FOLfunction(_, args), rest) ->
	CharSet.union (argument_variables(args)) (argument_variables(rest))
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
  function Arg(t) ->
    (match t with
	 Var(c) -> CharSet.add c CharSet.empty
       | FOLfunction(f, args) -> CharSet.add f (argument_symbols args)
       | Constant(c) -> CharSet.add c CharSet.empty)
    | Arguments(Var(c), rest) -> CharSet.add c (argument_symbols rest)
    | Arguments(Constant(c), rest) -> CharSet.add c (argument_symbols rest)
    | Arguments(FOLfunction(f, args), rest) ->
	CharSet.add f (CharSet.union (argument_symbols(args)) (argument_symbols(rest)))
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
    function Arg (Constant c) -> CharSet.add c CharSet.empty
      | Arg(FOLfunction(f, args)) -> cons_args(args)
      | Arg(_) -> CharSet.empty
      | Arguments(Constant c, rest) ->
	  CharSet.add c (cons_args rest)
      | Arguments(FOLfunction(f, args), rest) ->
	  CharSet.union (cons_args args) (cons_args rest)
      | Arguments(_, rest) -> cons_args rest
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
  function Arg(t) ->
    (match t with
	 Var(c) ->
	   (try
	      Arg(List.find (fun x -> x.v = c) subs).sv
	    with _ -> Arg(t))
       | FOLfunction(f, args) -> Arg(FOLfunction(f, apply_substitution subs args))
       | _ -> Arg(t))
    | Arguments(Var(c), rest) ->
	(try
	   Arguments((List.find (fun x -> x.v = c) subs).sv,
		     apply_substitution subs rest)
	with _ -> Arguments(Var(c), apply_substitution subs rest))
    | Arguments(Constant(c), rest) ->
	Arguments(Constant(c), apply_substitution subs rest)
    | Arguments(FOLfunction(f, args), rest) ->
	Arguments(FOLfunction(f, apply_substitution subs args),
		 apply_substitution subs rest)
;;
