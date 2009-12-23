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
(** Clause normal form *)

(** given a formula, it transforms the formula into clause normal form *)
val clause_normal_form : Fol.formula -> Fol.formula

(** list of clauses of a formula in clause normal form *)
val clauses : Fol.formula -> Fol.formula list

(** prints the constants and clauses of a formula in clause normal form *)
val print_clauses : Fol.formula -> unit
