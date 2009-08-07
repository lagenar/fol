type atomic_term = Var of char
		   | Constant of char;;
		   
(* non-empty arguments list of a function or predicate *)
type 'a arguments = Arg of 'a | Arguments of 'a * 'a arguments;;

(* a logical term is either an atomic term or a function *)
type term = Atm of atomic_term
	    | FOLfunction of char * term arguments;;

(* a first order logic formula *)
type formula = Atom of char * term arguments
	       | Or of formula * formula
	       | And of formula * formula
	       | Imp of formula * formula
	       | Neg of formula
	       | Forall of char * formula
	       | Exists of char * formula;;

let rec term_to_str t =
  match t with
      Atm(Var(c)) -> Char.escaped(c)
    | Atm(Constant(c)) -> Char.escaped(c)
    | FOLfunction(c, args) -> Char.escaped(c) ^ "(" ^ args_to_str(args) ^ ")"
and args_to_str args =
  match args with
      Arg(t) -> term_to_str(t)
    | Arguments(t, rest) -> term_to_str(t) ^ "," ^ args_to_str(rest);;

let rec formula_to_str formula =
  match formula with
      Atom(c, args) -> Char.escaped(c) ^ "(" ^ args_to_str(args) ^ ")"
    | Neg(f) -> (match f with
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

(* applies recursively the double negation simplification:
   ~~F = F*)
let rec double_negation_simplify formula =
  match formula with
      Neg(f) -> let r = double_negation_simplify(f) in
	(match r with
	    Neg(f) -> f
	  | form -> Neg(form))
    | Atom(c, args) -> Atom(c, args)
    | And(f1, f2) -> And(double_negation_simplify(f1),
			 double_negation_simplify(f2))
    | Or(f1, f2) -> Or(double_negation_simplify(f1),
		       double_negation_simplify(f2))
    | Imp(f1, f2) -> Imp(double_negation_simplify(f1),
			 double_negation_simplify(f2))
    | Forall(c, f) -> Forall(c, double_negation_simplify(f))
    | Exists(c, f) -> Exists(c, double_negation_simplify(f));;

(* De Morgan simplification:
   ~(a ^ b) = ~a v ~b
   ~(a v b) = ~a ^ ~b *)
let rec de_morgan_simplify formula =
  match formula with
      Neg(f) -> (match f with
		     And(f1, f2) -> Or(de_morgan_simplify(Neg(f1)),
				       de_morgan_simplify(Neg(f2)))
		   | Or(f1, f2) -> And(de_morgan_simplify(Neg(f1)),
				       de_morgan_simplify(Neg(f2)))
		   | Neg(form) -> de_morgan_simplify(form)
		   | form -> Neg(de_morgan_simplify(form)))
		       
    | Atom(c, args) -> Atom(c, args)
    | And(f1, f2) -> And(de_morgan_simplify(f1),
			 de_morgan_simplify(f2))
    | Or(f1, f2) -> Or(de_morgan_simplify(f1),
		       de_morgan_simplify(f2))
    | Imp(f1 ,f2) -> Imp(de_morgan_simplify(f1),
			 de_morgan_simplify(f2))	
    | Forall(c, f) -> Forall(c, de_morgan_simplify(f))
    | Exists(c, f) -> Exists(c, de_morgan_simplify(f));;

(* transforms implications applying the rule a -> b = ~a v b *)
let rec implication_simplify formula =
  match formula with
      Imp(f1, f2) -> Or(Neg(implication_simplify(f1)),
			implication_simplify(f2))
    | Atom(c, args) -> Atom(c, args)
    | And(f1, f2) -> And(implication_simplify(f1),
			 implication_simplify(f2))
    | Or(f1, f2) -> Or(implication_simplify(f1),
			implication_simplify(f2))
    | Neg(f) -> Neg(implication_simplify(f))
    | Forall(c, f) -> Forall(c, implication_simplify(f))
    | Exists(c, f) -> Exists(c, implication_simplify(f));;

(* ~ForAll(x)A(x) = Exists(x)~A(x)
   ~Exists(x)A(x) = ForAll(x)~A(x) *)
let rec negated_quantifiers_simplify formula =
  match formula with
      Neg(subform) -> let r = negated_quantifiers_simplify(subform) in
	(match r with
	     Forall(c, f) -> Exists(c, negated_quantifiers_simplify(Neg(f)))
	   | Exists(c, f) -> Forall(c, negated_quantifiers_simplify(Neg(f)))
	   | form -> Neg(form))
	  
    | Atom(c, args) -> Atom(c, args)
    | And(f1, f2) -> And(negated_quantifiers_simplify(f1),
			 negated_quantifiers_simplify(f2))
    | Or(f1, f2) -> Or(negated_quantifiers_simplify(f1),
		       negated_quantifiers_simplify(f2))
    | Imp(f1, f2) -> Imp(negated_quantifiers_simplify(f1),
			 negated_quantifiers_simplify(f2))
    | Forall(c, f) -> Forall(c, negated_quantifiers_simplify(f))
    | Exists(c, f) -> Exists(c, negated_quantifiers_simplify(f));;

let negation_normal_form formula =
  double_negation_simplify(de_morgan_simplify(
			     implication_simplify(
			       negated_quantifiers_simplify(formula))));;
