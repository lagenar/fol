(* non-empty arguments list of a function or predicate *)
type 'a arguments = Arg of 'a | Arguments of 'a * 'a arguments;;

(* a logical term is either an atomic term or a function *)
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
	  Atom _ | Quantifier _ -> "~" ^ formula_to_str(f)
	| _ -> "~" ^ "(" ^ formula_to_str(f) ^ ")")
    | Connective(And, f1, f2) -> 
	let paren f = (match f with
			   Connective(Imp, _, _)
			 | Connective(Or, _, _) -> "(" ^ formula_to_str(f) ^ ")"
			 | _ -> formula_to_str(f)) in
	  paren(f1) ^ "^" ^ paren(f2)
    | Connective(Or, f1, f2) ->
	let paren f = (match f with
			   Connective(Imp, _, _) -> "(" ^ formula_to_str(f) ^ ")"
			 | _ -> formula_to_str(f)) in
	  paren(f1) ^ "v" ^ paren(f2)
    | Connective(Imp, f1, f2) -> formula_to_str(f1) ^ "=>" ^ formula_to_str(f2)
    | Quantifier(Exists, c, f) -> "Exists(" ^ Char.escaped(c) ^ ")" ^ "(" ^ formula_to_str(f) ^ ")"
    | Quantifier(Forall, c, f) -> "Forall(" ^ Char.escaped(c) ^ ")" ^ "(" ^ formula_to_str(f) ^ ")";;

(* transforms implications applying the rule a -> b = ~a v b *)
let rec implication_simplify formula =
  match formula with
      Connective(Imp, f1, f2) -> Connective(Or, Not(implication_simplify(f1)),
					    implication_simplify(f2))
    | Connective(c, f1, f2) -> Connective(c, implication_simplify(f1),
					  implication_simplify(f2))
    | Not(f) -> Not(implication_simplify(f))
    | Quantifier(q, c, f) -> Quantifier(q, c, implication_simplify(f))
    | _ -> formula
;;

let rec move_not_inwards  =
  function
      Connective(c, f1, f2) -> Connective(c, move_not_inwards(f1), move_not_inwards(f2))
    | Quantifier(q, c, f) -> Quantifier(q, c, move_not_inwards(f))
    | Not(Connective(And, f1, f2)) ->
	Connective(Or, move_not_inwards(Not(f1)), move_not_inwards(Not(f2)))
    | Not(Connective(Or, f1, f2)) ->
	Connective(And, move_not_inwards(Not(f1)), move_not_inwards(Not(f2)))
    | Not(Quantifier(Forall, c, f)) -> Quantifier(Exists, c, move_not_inwards(Not(f)))
    | Not(Quantifier(Exists, c, f)) -> Quantifier(Forall, c, move_not_inwards(Not(f)))
    | Not(Not(f)) -> move_not_inwards(f)
    | Not(f) -> Not(move_not_inwards(f))
    | f -> f
;;

let negation_normal_form formula =
  move_not_inwards(implication_simplify(formula));;

module CharSet = Set.Make(struct type t = char let compare = compare end);;

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

let rec free_variables =
  function Atom(c, args) -> argument_variables(args)
    | Connective(c, f1, f2)  ->
	CharSet.union (free_variables f1) (free_variables f2)	
    | Not(f) -> free_variables(f)
    | Quantifier(q, c, f) -> CharSet.remove c (free_variables f)
;;

let rec miniscope =
  function
      Quantifier(q, c, f) ->
	let mf = miniscope f in
	  (match mf with
	       Connective(con, f1, f2) ->
		 if not (CharSet.mem c (free_variables f2)) then
		   Connective(con, miniscope(Quantifier(q, c, f1)), f2)
		 else if not (CharSet.mem c (free_variables f1)) then
		   Connective(con, f1, miniscope(Quantifier(q, c, f2)))
		 else
		   Connective(con, miniscope(Quantifier(q, c, f1)), miniscope(Quantifier(q, c, f2)))
	     | _ -> Quantifier(q, c, mf))
    | Connective(c, f1, f2) -> Connective(c, miniscope(f1), miniscope(f2))
    | Not(f) -> Not(miniscope(f))
    | f -> f
;;

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

let rec term_symbols  =
  function Atom(c, args) -> argument_symbols(args)
    | Connective(c, f1, f2) ->
	CharSet.union (term_symbols f1) (term_symbols f2)
    | Not(f) -> term_symbols(f)
    | Quantifier(q, c, f) -> term_symbols f
;;

let alpha_set =
  List.fold_right (CharSet.add)
    ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';
     'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']
    CharSet.empty
;;
  
