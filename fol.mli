(*
 * fol
 * Copyright (C) 2009 Lucas Moauro
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
(** First order logic definitions *)

(** a logical term is either a variable or a function(constants are 0-ary functions *)
type term = Var of string
	    | FOLfunction of string * term list;;

type connective = And | Or | Imp;;
type quantifier = Exists | Forall;;

(** a first order logic formula *)
type formula = Atom of string * term list
	       | Connective of connective * formula * formula
	       | Not of formula
	       | Quantifier of quantifier * string * formula
;;

(** arity of a function or predicate *)
val arity : term list -> int

(** string representation of a formula *)
val formula_to_str : formula -> string

(** set of variable identifiers of a function or predicate *)
val argument_variables : term list -> Util.StringSet.t

(** set of free variable identifiers *)
val free_variables : formula -> Util.StringSet.t

(** set of bound variable identifiers *)
val bound_variables : formula -> Util.StringSet.t

(** set of function, variable and constant identifiers in the arguments of a predicate or function*)
val argument_identifiers : term list -> Util.StringSet.t

(** set of function, variable and constant identifiers in a formula *)
val term_identifiers : formula -> Util.StringSet.t

(** set of constant identifiers in a formula *)
val constants : formula -> Util.StringSet.t

(** a variable to term substitution *)
type substitution = { v: string; sv : term }

(** applies a substitution to a variable *)
val apply : substitution list -> string -> term

(** check if a variable to term substitution is defined *)
val defined : substitution list -> string -> bool

(** applies a list of substitutions to a predicate's arguments *)
val apply_substitution : substitution list -> term list -> term list

