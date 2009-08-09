(* non-empty arguments list of a function or predicate *)
type 'a arguments = Arg of 'a | Arguments of 'a * 'a arguments;;

(* a logical term is either an atomic term or a function *)
type term = Var of char
	    | Constant of char
	    | FOLfunction of char * term arguments;;

(* a first order logic formula *)
type formula = Atom of char * term arguments
	       | Or of formula * formula
	       | And of formula * formula
	       | Imp of formula * formula
	       | Not of formula
	       | Forall of char * formula
	       | Exists of char * formula;;

let rec term_to_str t =
  match t with
      Var(c) -> Char.escaped(c)
    | Constant(c) -> Char.escaped(c)
    | FOLfunction(c, args) -> Char.escaped(c) ^ "(" ^ args_to_str(args) ^ ")"
and args_to_str args =
  match args with
      Arg(t) -> term_to_str(t)
    | Arguments(t, rest) -> term_to_str(t) ^ "," ^ args_to_str(rest);;

let rec formula_to_str formula =
  match formula with
      Atom(c, args) -> Char.escaped(c) ^ "(" ^ args_to_str(args) ^ ")"
    | Not(f) -> (match f with
	  Atom _ | Forall _ | Exists _ -> "~" ^ formula_to_str(f)
	| other -> "~" ^ "(" ^ formula_to_str(f) ^ ")")
    | And(f1, f2) -> 
	let paren f = (match f with
			   Imp(_) | Or(_) -> "(" ^ formula_to_str(f) ^ ")"
			 | _ -> formula_to_str(f)) in
	  paren(f1) ^ "^" ^ paren(f2)
    | Or(f1, f2) ->
	let paren f = (match f with
			   Imp(_) -> "(" ^ formula_to_str(f) ^ ")"
			 | _ -> formula_to_str(f)) in
	  paren(f1) ^ "v" ^ paren(f2)
    | Imp(f1, f2) -> formula_to_str(f1) ^ "=>" ^ formula_to_str(f2)
    | Exists(c, f) -> "Exists(" ^ Char.escaped(c) ^ ")" ^ "(" ^ formula_to_str(f) ^ ")"
    | Forall(c, f) -> "Forall(" ^ Char.escaped(c) ^ ")" ^ "(" ^ formula_to_str(f) ^ ")";;

(* transforms implications applying the rule a -> b = ~a v b *)
let rec implication_simplify formula =
  match formula with
      Imp(f1, f2) -> Or(Not(implication_simplify(f1)),
			implication_simplify(f2))
    | Atom(c, args) -> Atom(c, args)
    | And(f1, f2) -> And(implication_simplify(f1),
			 implication_simplify(f2))
    | Or(f1, f2) -> Or(implication_simplify(f1),
			implication_simplify(f2))
    | Not(f) -> Not(implication_simplify(f))
    | Forall(c, f) -> Forall(c, implication_simplify(f))
    | Exists(c, f) -> Exists(c, implication_simplify(f));;

let rec move_not_inwards  =
  function
      And(f1,f2) -> And(move_not_inwards(f1), move_not_inwards(f2))
    | Or(f1,f2) -> Or(move_not_inwards(f1), move_not_inwards(f2))
    | Imp(f1,f2) -> Imp(move_not_inwards(f1), move_not_inwards(f2))
    | Forall(c,f) -> Forall(c, move_not_inwards(f))
    | Exists(c,f) -> Exists(c, move_not_inwards(f))
    | Not(And(f1,f2)) -> Or(move_not_inwards(Not(f1)), move_not_inwards(Not(f2)))
    | Not(Or(f1,f2)) -> And(move_not_inwards(Not(f1)), move_not_inwards(Not(f2)))
    | Not(Forall(c,f)) -> Exists(c, move_not_inwards(Not(f)))
    | Not(Exists(c,f)) -> Forall(c, move_not_inwards(Not(f)))
    | Not(Not(f)) -> move_not_inwards(f)
    | Not(f) -> Not(move_not_inwards(f))
    | f -> f
;;

let negation_normal_form formula =
  move_not_inwards(implication_simplify(formula));;

module VarsSet = Set.Make(struct type t = char let compare = compare end);;

let rec argument_variables =
  function Arg(t) ->
    (match t with
	 Var(c) -> VarsSet.add c VarsSet.empty
       | FOLfunction(_, args) -> argument_variables args
       | _ -> VarsSet.empty)
    | Arguments(Var(c), rest) -> VarsSet.add c (argument_variables rest)
    | Arguments(Constant(c), rest) -> argument_variables rest
    | Arguments(FOLfunction(_, args), rest) ->
	VarsSet.union (argument_variables(args)) (argument_variables(rest))
;;

let rec free_variables =
  function Atom(c, args) -> argument_variables(args)
    | And(f1, f2)
    | Or(f1, f2)
    | Imp(f1, f2) -> VarsSet.union (free_variables f1) (free_variables f2)
    | Not(f) -> free_variables(f)
    | Exists(c, f) 
    | Forall(c, f) -> VarsSet.remove c (free_variables f)
;;

let rec miniscope =
  function
      Forall(c, f) ->
	let mf = miniscope f in
	  (match mf with
	       And(f1, f2) ->
		 if not (VarsSet.mem c (free_variables f2)) then And(miniscope(Forall(c, f1)), f2)
		 else if not (VarsSet.mem c (free_variables f1)) then And(f1, miniscope(Forall(c, f2)))
		 else And(miniscope(Forall(c, f1)), miniscope(Forall(c, f2)))
	     | Or(f1, f2) ->
		 if not (VarsSet.mem c (free_variables f2)) then Or(miniscope(Forall(c, f1)), f2)
		 else if not (VarsSet.mem c (free_variables f1)) then Or(f1, miniscope(Forall(c, f2)))
		 else Or(miniscope(Forall(c, f1)), miniscope(Forall(c, f2)))
	     | _ -> Forall(c, mf))
    | Exists(c, f) ->
	let mf = miniscope f in
	  (match mf with
	       And(f1, f2) ->
		 if not (VarsSet.mem c (free_variables f2)) then And(miniscope(Exists(c, f1)), f2)
		 else if not (VarsSet.mem c (free_variables f1)) then And(f1, miniscope(Exists(c, f2)))
		 else And(miniscope(Exists(c, f1)), miniscope(Exists(c, f2)))
	     | Or(f1, f2) ->
		 if not (VarsSet.mem c (free_variables f2)) then Or(miniscope(Exists(c, f1)), f2)
		 else if not (VarsSet.mem c (free_variables f1)) then Or(f1, miniscope(Exists(c, f2)))
		 else Or(miniscope(Exists(c, f1)), miniscope(Exists(c, f2)))
	     | _ -> Exists(c, mf))
    | Atom(_, _) as a -> a
    | Not(f) -> Not(miniscope(f))
    | And(f1, f2) -> And(miniscope(f1), miniscope(f2))
    | Or(f1, f2) -> Or(miniscope(f1), miniscope(f2))
;;
