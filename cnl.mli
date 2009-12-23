(** Clause normal form *)

(** given a formula, it transforms the formula into clause normal form *)
val clause_normal_form : Fol.formula -> Fol.formula

(** list of clauses of a formula in clause normal form *)
val clauses : Fol.formula -> Fol.formula list

(** prints the constants and clauses of a formula in clause normal form *)
val print_clauses : Fol.formula -> unit
