(** First order logic definitions *)

(** non-empty argument list of a function or predicate *)
type 'a arguments = Arg of 'a | Arguments of 'a * 'a arguments;;

(** a logical term is either(variable or constant) an atomic term or a function *)
type term = Var of char
	    | Constant of char
	    | FOLfunction of char * term arguments;;

type connective = And | Or | Imp;;
type quantifier = Exists | Forall;;

(** a first order logic formula *)
type formula = Atom of char * term arguments
	       | Connective of connective * formula * formula
	       | Not of formula
	       | Quantifier of quantifier * char * formula
;;

(** arity of a function or predicate *)
val arity : 'a arguments -> int

(** string representation of a formula *)
val formula_to_str : formula -> string

(** set of variable symbols of a function or predicate *)
val argument_variables : term arguments -> Util.CharSet.t

(** set of free variable symbols *)
val free_variables : formula -> Util.CharSet.t

(** set of bound variable symbols *)
val bound_variables : formula -> Util.CharSet.t

(** set of function, variable and constant symbols in the arguments of a predicate or function*)
val argument_symbols : term arguments -> Util.CharSet.t

(** set of function, variable and constant symbols in a formula *)
val term_symbols : formula -> Util.CharSet.t

(** set of constant symbols in a formula *)
val constants : formula -> Util.CharSet.t

(** a variable to term substitution *)
type substitution = { v: char; sv : term }

(** applies a list of substitutions to a predicate's arguments *)
val apply_substitution : substitution list -> term arguments -> term arguments

