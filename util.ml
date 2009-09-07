module CharSet = Set.Make(struct type t = char let compare = compare end);;

let alpha_set =
  List.fold_right CharSet.add
    ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';
     'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']
    CharSet.empty
;;

