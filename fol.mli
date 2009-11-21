(** First order logic definitions *)

(** a logical term is either a variable or a function(constants are 0-ary functions *)
type term = Var of char
	    | FOLfunction of char * term list;;

type connective = And | Or | Imp;;
type quantifier = Exists | Forall;;

(** a first order logic formula *)
type formula = Atom of char * term list
	       | Connective of connective * formula * formula
	       | Not of formula
	       | Quantifier of quantifier * char * formula
;;

(** arity of a function or predicate *)
val arity : term list -> int

(** string representation of a formula *)
val formula_to_str : formula -> string

(** set of variable symbols of a function or predicate *)
val argument_variables : term list -> Util.CharSet.t

(** set of free variable symbols *)
val free_variables : formula -> Util.CharSet.t

(** set of bound variable symbols *)
val bound_variables : formula -> Util.CharSet.t

(** set of function, variable and constant symbols in the arguments of a predicate or function*)
val argument_symbols : term list -> Util.CharSet.t

(** set of function, variable and constant symbols in a formula *)
val term_symbols : formula -> Util.CharSet.t

(** set of constant symbols in a formula *)
val constants : formula -> Util.CharSet.t

(** a variable to term substitution *)
type substitution = { v: char; sv : term }

(** applies a substitution to a variable *)
val apply : substitution list -> char -> term

(** check if a variable to term substitution is defined *)
val defined : substitution list -> char -> bool

(** applies a list of substitutions to a predicate's arguments *)
val apply_substitution : substitution list -> term list -> term list

