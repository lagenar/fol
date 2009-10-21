
open Fol
open Util

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

(* Existentially quantifies the appearences of free variables in
   a formula. Producing a new formula that preserves satisfiability.
*)
let quantify_free_variables formula =
  let quant_free free_vars f =
    CharSet.fold (fun c t -> Quantifier(Exists, c, t)) free_vars f
  in
    quant_free (free_variables formula) formula
;;

(* A logical formula is in negation normal form if negation occurs
   only immediately above elementary propositions and {~, v, ^} are
   the only allowed Boolean connectives.
*)
let negation_normal_form formula =
  quantify_free_variables
    (move_not_inwards
       (implication_simplify formula))
;;
  
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

(* renames variables such that the ocurrences of
   cuantifiers bind different variable symbols *)
let rename_variables unused_symbols formula =
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
    ren formula [] unused_symbols
;;

let unused_symbols constant_symbols formula =
  CharSet.elements (CharSet.diff alpha_set
		      (CharSet.union constant_symbols
			 (term_symbols formula)))
;;

(* Removes existential quantifiers by replacing
   variables existentially quantified with new function
   or constant symbols
*)
let skolemize constant_symbols formula =
  let rec list_to_args l =
    List.map (fun x -> Var x) l
  in
   let rec skol f bound_vars subs unused =
    match f with
	Atom(c, args) ->
	    (Atom(c, apply_substitution subs  args), unused)
      | Connective(c, f1, f2) ->
	  let r1 = skol f1 bound_vars subs unused in
	  let r2 = skol f2 bound_vars subs (snd r1) in
	    (Connective(c, fst r1, fst r2), snd r2)
      | Not(f) ->
	  let r = skol f bound_vars subs unused in
	    (Not(fst r), snd r)
      | Quantifier(Forall, c, f) ->
	  let r = skol f (c::bound_vars) subs unused in
	    (Quantifier(Forall, c, fst r), snd r)
      | Quantifier(Exists, c, f) ->
	  let t = FOLfunction(List.hd unused, list_to_args bound_vars) in
	    skol f bound_vars ({v=c; sv=t}::subs) (List.tl unused)
   in
   let ren_f, unused = rename_variables (unused_symbols constant_symbols formula) formula
   in
     fst(skol ren_f [] [] unused)
;;

(* moves quantifiers outwards *)  
let rec move_quant_outwards  =
  function
      Atom(_, _) as a -> a
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

let clause_normal_form (formula, constant_symbols) =
  distribute_or (
    move_quant_outwards
      ( skolemize constant_symbols
	      ( miniscope (
		  negation_normal_form formula))))
;;

let rec clauses formula =
  match formula with
      Connective(And, f1, f2) -> (clauses f1) @ (clauses f2)
    | Connective(Or, f1, f2) as f -> [f]
    | Atom(_) as p -> [p]
    | Not(_) as f -> [f]
    | Quantifier(Forall(_), c, f) -> clauses(f)
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
