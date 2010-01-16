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
open Util

type term = Var of string
	    | FOLfunction of string * term list;;

type connective = And | Or | Imp;;
type quantifier = Exists | Forall;;

(* A first order logic formula *)
type formula =   Atom of string * term list
	       | Connective of connective * formula * formula
	       | Not of formula
	       | Quantifier of quantifier * string * formula
;;

(* Number of arguments of a function or predicate *)
let rec arity (args : term list) =
  List.length args
;;

let rec term_to_str t =
  match t with
      Var(v) -> v
    | FOLfunction(f, []) -> f
    | FOLfunction(f, args) -> f ^ "(" ^ args_to_str(args) ^ ")"
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
	Atom(atom, []) -> atom
      |	Atom(atom, args) -> atom ^ "(" ^ args_to_str(args) ^ ")"
      | Not(f) -> "~" ^ paren f
      | Connective(And, f1, f2) -> 
	  paren f1 ^ " & " ^ paren f2	  
      | Connective(Or, f1, f2) ->
	    paren f1 ^ " | " ^ paren f2
      | Connective(Imp, f1, f2) -> paren f1 ^ " => " ^ paren f2
      | Quantifier(Exists, v, f) -> "Exists[" ^ v ^ "]" ^ "(" ^ formula_to_str(f) ^ ")"
      | Quantifier(Forall, v, f) -> "Forall[" ^ v ^ "]" ^ "(" ^ formula_to_str(f) ^ ")"
;;

(* Set of variable identifiers that appear in the arguments of a function or predicate *)
let rec argument_variables =
  function
      [] -> StringSet.empty
    | Var(x)::xs -> StringSet.add x (argument_variables xs)
    | FOLfunction(f, args)::xs ->
	StringSet.union (argument_variables args) (argument_variables xs)
;;

(* Set of free variable identifiers(that are not in
   a quantifier's scope) *)
let rec free_variables =
  function
      Atom(_, args) -> argument_variables(args)
    | Connective(c, f1, f2)  ->
	StringSet.union (free_variables f1) (free_variables f2)	
    | Not(f) -> free_variables(f)
    | Quantifier(q, v, f) -> StringSet.remove v (free_variables f)
;;

(* Set of bound variable identifiers *)
let rec bound_variables =
  function
    | Connective(c, f1, f2) ->
	StringSet.union (bound_variables f1) (bound_variables f2)
    | Not(f) -> bound_variables f
    | Quantifier(q, v, f) -> StringSet.add v (bound_variables f)
    | _ -> StringSet.empty
;;

(* Set of identifiers(functions, vars and constants)
   used as arguments to a predicate *)
let rec argument_identifiers =
    function
        [] -> StringSet.empty
      | Var(x)::xs -> StringSet.add x (argument_identifiers xs)
      | FOLfunction(f, args)::xs ->
	  StringSet.add f (StringSet.union (argument_identifiers args) (argument_identifiers xs))
;;

(* Set of function, variable and constant identifiers
   used in a formula *)
let rec term_identifiers  =
  function
      Atom(_, args) -> argument_identifiers(args)
    | Connective(c, f1, f2) ->
	StringSet.union (term_identifiers f1) (term_identifiers f2)
    | Not(f) -> term_identifiers(f)
    | Quantifier(q, v, f) -> StringSet.add v (term_identifiers f)
;;

(* Set of constant identifiers *)
let rec constants formula =
  let rec cons_args =
    function
      | [] -> StringSet.empty
      | Var(x)::xs -> StringSet.empty
      | FOLfunction(f, [])::xs ->
	  StringSet.add f (cons_args xs)
      | FOLfunction(f, args)::xs ->
	  StringSet.union (cons_args args) (cons_args xs)
  in
    match formula with
	Not(f) -> constants(f)
      | Atom(_, args) -> cons_args args
      | Quantifier(_, _, f) -> constants f
      | Connective(_, f1, f2) ->
	  StringSet.union (constants f1) (constants f2)
;;

type substitution = {v : string; sv : term};;

(* Applies a substitution to a variable *)
let apply subs x = (List.find (fun s -> s.v = x) subs).sv

(* Check if a variable to term substitution is defined *)
let defined subs x = List.exists (fun s -> s.v = x) subs;;

(* Applies a list of variable to term substitution
   to a list of terms *)
let rec apply_substitution subs =
  function
      [] -> []
    | Var(x)::xs ->
	(try apply subs x::apply_substitution subs xs
	 with _ -> Var(x)::apply_substitution subs xs)
    | FOLfunction(f, args)::xs ->
	FOLfunction(f, apply_substitution subs args)::apply_substitution subs xs
;;
