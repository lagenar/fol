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
(** Utils *)

let rec zip l1 l2 =
  match (l1, l2) with
      ([], []) -> []
    | (x1::x1s, x2::x2s) -> (x1,x2)::zip x1s x2s
    | _ -> failwith "length of lists mismatch";;

module StringSet = Set.Make (struct type t = string let compare = compare end);;
