(* non-empty arguments list of a function or predicate *)
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

module CharSet = Set.Make(struct type t = char let compare = compare end);;

(*number of arguments of a function or predicate*)
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
	  paren(f1) ^ " ^ " ^ paren(f2)
    | Connective(Or, f1, f2) ->
	let paren f = (match f with
			   Connective(Imp, _, _) -> "(" ^ formula_to_str(f) ^ ")"
			 | _ -> formula_to_str(f)) in
	  paren(f1) ^ " v " ^ paren(f2)
    | Connective(Imp, f1, f2) -> formula_to_str(f1) ^ " => " ^ formula_to_str(f2)
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

(*applies the rules:
  double negation: ~~a = a
  negated quantifiers:
  ~Exists(x)P(x) = Forall(x)~P(x)
  ~Forall(x)P(x) = Exists(x)~P(x)
  de morgan laws:
  ~(a v b) = ~a ^ ~b
  ~(a ^ b) = ~a v ~b
*)
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

(*Set of variable simbols that appear in the arguments*)
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

(* Existentially quantifies the appearences of free variables in
   a formula. Producing a new formula that preserves satisfiability
   but not necessarily preserves equivalence *)
let rec quantify_free_variables formula bound_vars =
  let quant_free free_vars f =
    CharSet.fold (fun c t -> Quantifier(Exists, c, t)) free_vars f
  in
    match formula with
	Atom(c, args) as p ->
	  quant_free (CharSet.diff (free_variables p) bound_vars) p
      | Not(f) -> Not (quantify_free_variables f bound_vars)
      | Connective(c, f1, f2) ->
	  Connective(c, (quantify_free_variables f1 bound_vars),
		     (quantify_free_variables f2 bound_vars))
      | Quantifier(q, c, f) ->
	  Quantifier(q, c, quantify_free_variables f (CharSet.add c bound_vars))
;;

(* A logical formula is in negation normal form if negation occurs
   only immediately above elementary propositions and {~, v, ^} are
   the only allowed Boolean connectives
*)
let negation_normal_form formula =
  quantify_free_variables
    (move_not_inwards
       (implication_simplify formula))
    CharSet.empty;;
  
(* Rules:
   Exists(x)(f1 v f2) = Exists(x)(f1) v f2  if not x in free(f2)
   Exists(x)(f1 ^ f2) = Exists(x)(f1) ^ f2  if not x in free(f2)
   Forall(x)(f1 v f2) = Forall(x)(f1) v f2  if not x in free(f2)
   Forall(x)(f1 ^ f2) = Forall(x)(f1) ^ f2  if not x in free(f2)

   Forall(x)(f1 ^ f2) = Forall(x)(f1) ^ Forall(x)(f2)
   if x in free(f1) and x in free(f2)

   Exists(x)(f1 v f2) = Exists(x)(f1) v Exists(x)(f2)
   if x in free(f1) and x in free(f2)

   The aim of the rules is to minimize the arity of skolem
   functions by moving quantifiers as inwards as possible
*)
let rec miniscope =
  function
      Quantifier(q, c, f) ->
	let mf = miniscope f in
	  (match mf with
	       Connective(con, f1, f2) ->
		 let freef1 = free_variables f1 and
		     freef2 = free_variables f2 in
		   if not (CharSet.mem  c freef1) && not (CharSet.mem c freef2) then
		     Connective(con, miniscope(f1), miniscope(f2))
		   else if not (CharSet.mem c freef2) then
		     Connective(con, miniscope(Quantifier(q, c, f1)), f2)
		   else if not (CharSet.mem c freef1) then
		     Connective(con, f1, miniscope(Quantifier(q, c, f2)))
		   else
		     (match (q,con) with
			  (Forall, And) ->
			    Connective(And, miniscope(Quantifier(Forall, c, f1))
					 , miniscope(Quantifier(Forall, c, f2)))
			| (Exists, Or) ->
			    Connective(Or, miniscope(Quantifier(Exists, c, f1))
					 , miniscope(Quantifier(Exists, c, f2)))
			| _ -> Quantifier(q, c, mf))
             | _ -> Quantifier(q, c, mf))
    | Connective(c, f1, f2) -> Connective(c, miniscope(f1), miniscope(f2))
    | Not(f) -> Not(miniscope(f))
    | f -> f
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

(* set of function, variables and constants symbols
   used in a formula *)
let rec term_symbols  =
  function Atom(c, args) -> argument_symbols(args)
    | Connective(c, f1, f2) ->
	CharSet.union (term_symbols f1) (term_symbols f2)
    | Not(f) -> term_symbols(f)
    | Quantifier(q, c, f) -> CharSet.add c (term_symbols f)
;;

let alpha_set =
  List.fold_right CharSet.add
    ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';
     'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']
    CharSet.empty
;;

type substitution = {v : char; sv : term};;

(* applies a list of variable to term substitution
   to the predicates arguments*)
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

(* renames variables such that the ocurrences of
   cuantifiers bind different variable symbols *)
let rename_variables formula =
  let rec ren formula subs unused_symbols =
    match formula with
	Atom(c, args) -> (Atom(c, apply_substitution subs args), unused_symbols)
      | Connective(c, f1, f2) ->
	  let r1 = ren f1 subs unused_symbols in
	  let r2 = ren f2 subs (snd r1) in
	    (Connective(c, fst r1, fst r2), snd r2)
      | Not(f) ->
	  let r = ren f subs unused_symbols in
	    (Not(fst r), snd r)
      | Quantifier(q, c, f) ->
	  let sub = {v=c; sv=Var(List.hd unused_symbols)} in
	  let r = ren f (sub::subs) (List.tl unused_symbols) in
	    (Quantifier(q, List.hd unused_symbols, fst r), snd r)
  in
  fst ( ren formula [] (CharSet.elements(CharSet.diff alpha_set (term_symbols formula))))
;;

(* Removes existential quantifiers by replacing
   variables existentially quantified with new function
   or constant symbols
*)
let skolemize formula =
  let rec list_to_args l =
    if (List.tl l) = [] then Arg(Var(List.hd l))
    else Arguments(Var(List.hd l), list_to_args (List.tl l))
  in
  let rec skol f bound_vars subs unused_symbols =
    match f with
	Atom(c, args) ->
	    (Atom(c, apply_substitution subs  args), unused_symbols)
      | Connective(c, f1, f2) ->
	  let r1 = skol f1 bound_vars subs unused_symbols in
	  let r2 = skol f2 bound_vars subs (snd r1) in
	    (Connective(c, fst r1, fst r2), snd r2)
      | Not(f) ->
	  let r = skol f bound_vars subs unused_symbols in
	    (Not(fst r), snd r)
      | Quantifier(Forall, c, f) ->
	  let r = skol f (c::bound_vars) subs unused_symbols in
	    (Quantifier(Forall, c, fst r), snd r)
      | Quantifier(Exists, c, f) ->
	  let t =
	    if bound_vars = [] then
	      Constant(List.hd unused_symbols)
	    else
	       FOLfunction(List.hd unused_symbols, list_to_args bound_vars) in
	    skol f bound_vars ({v=c; sv=t}::subs) (List.tl unused_symbols)
  in
    fst(skol formula []
	  [] (CharSet.elements(CharSet.diff alpha_set (term_symbols formula))))
;;

(* moves quantifiers outwards *)  
let rec move_quant_outwards  =
  function
      Atom(c, f) as a -> a
    | Not(f) -> Not(move_quant_outwards f)
    | Connective(con, Quantifier(q, c, f1), f2)
    | Connective(con, f2, Quantifier(q, c, f1)) ->
	let r1 = move_quant_outwards f1 in
	let r2 = move_quant_outwards f2 in
	  if not (CharSet.mem c (free_variables r2)) then
	    Quantifier(q, c, move_quant_outwards(Connective(con, r1, r2)))
	  else
	    Connective(con, r1, r2)
    | Connective(c, f1, f2) ->
	let r1 = move_quant_outwards f1 in
	let r2 = move_quant_outwards f2 in
	  (match (r1, r2) with
	       (Quantifier(_,_,_), _)
	     | (_, Quantifier(_,_,_)) -> move_quant_outwards (Connective(c, r1, r2))
	     | _ -> Connective(c, r1, r2))
    | Quantifier(q, c, f) -> Quantifier(q, c, move_quant_outwards f)
;;

(* Rules:
   f1 v (f2 ^ f3) = (f1 v f2) ^ (f1 v f3)
*)
let rec distribute_or =
  function
      Atom(_, _) as a -> a
    | Not(f) -> Not(distribute_or(f))
    | Connective(Or, f1, f2) ->
	let r1 = distribute_or f1 in
	let r2 = distribute_or f2 in
	  (match (r1, r2) with
	       (_, Connective(And, s1, s2)) ->
		 Connective(And, distribute_or(Connective(Or, r1, s1))
			      , distribute_or(Connective(Or, r1, s2)))
	     | (Connective(And, s1, s2), _) ->
		 Connective(And, distribute_or(Connective(Or, r2, s1))
			      , distribute_or(Connective(Or, r2, s2)))
	     | _ -> Connective(Or, r1, r2))
    | Connective(c, f1, f2) ->
	Connective(c, distribute_or f1, distribute_or f2)
    | Quantifier(q, c, f) ->
	Quantifier(q, c, distribute_or f)
;;

let clause_normal_form formula =
  distribute_or (
    move_quant_outwards
      ( skolemize (
	  miniscope (
	    negation_normal_form formula))))
;;

(* clause normal form formula's list of clauses *)					 
let rec clauses formula =
  match formula with
      Connective(And, f1, f2) -> (clauses f1) @ (clauses f2)
    | Connective(Or, f1, f2) as f -> [f]
    | Atom(_) as p -> [p]
    | Not(_) as f -> [f]
    | Quantifier(Forall(_), c, f) -> clauses(f)
;;

(* set of constants symbols *)
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

let print_clauses formula =
  let print_constants lc =
    if lc = [] then print_endline "Const()"
    else begin
      Printf.printf "Const(%c" (List.hd lc);
      List.iter (fun x -> Printf.printf ",%c" x) (List.tl lc);
      print_endline ")"
    end
  in
    print_constants (CharSet.elements (constants formula));
    let cls = clauses formula in
      List.iter (fun x -> print_endline(formula_to_str x)) cls
;;
