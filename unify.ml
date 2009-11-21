open Fol
open Util
open List

let rec is_triv subs x t =
  match t with
      Var y -> y = x or defined subs y & is_triv subs x (apply subs y)
    | FOLfunction(f, args) ->
	exists (fun t -> is_triv subs x t) args & failwith "cyclic";;

let rec unify_terms subs eqs = 
  match eqs with
      [] -> subs
    | (FOLfunction(f, fargs), FOLfunction(g, gargs))::xs ->
	if f = g & length fargs = length gargs
	then unify_terms subs (zip fargs gargs @ xs)
	else failwith "impossible unification"
    | (Var x, t)::xs ->
	if defined subs x then unify_terms subs ((apply subs x, t)::xs)
	else unify_terms (if is_triv subs x t then subs else {v=x;sv=t}::subs) xs
    | (t, Var x)::xs -> unify_terms subs ((Var x, t)::xs);;

