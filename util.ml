(** Utils *)

let rec zip l1 l2 =
  match (l1, l2) with
      ([], []) -> []
    | (x1::x1s, x2::x2s) -> (x1,x2)::zip x1s x2s
    | _ -> failwith "length of lists mismatch";;

module StringSet = Set.Make (struct type t = string let compare = compare end);;
