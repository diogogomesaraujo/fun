(** Module that implements the symbol table lookup function.*)

(** [lookup x l] searches in l for a value equal to x and panics if it is not able to find it.*)
let lookup x l =
  List.find_index (fun e -> e = x) l |> Option.get
