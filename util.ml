(** Utils *)

let rec zip l1 l2 =
  match (l1, l2) with
      ([], []) -> []
    | (x1::x1s, x2::x2s) -> (x1,x2)::zip x1s x2s
    | _ -> failwith "length of lists mismatch";;

(** set of characters *)
module CharSet = Set.Make(struct type t = char let compare = compare end);;

let alpha_set =
  List.fold_right CharSet.add
    ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';
     'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']
    CharSet.empty
;;

