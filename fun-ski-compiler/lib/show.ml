open Combinator

let rec combinator_to_string c =
  match c with
  | Variable v -> v
  | Constant n -> Printf.sprintf "%d" n
  | S -> "S"
  | K -> "K"
  | I -> "I"
  | B -> "B"
  | C -> "C"
  | Y c' -> "Y" ^ combinator_to_string c'
  | Application (c1, c2) ->
    Printf.sprintf "(%s %s)"
      (combinator_to_string c1)
      (combinator_to_string c2)
  | Primitive (Addition (c1, c2)) ->
    Printf.sprintf "(%s + %s)"
      (combinator_to_string c1)
      (combinator_to_string c2)
  | Primitive (Subtraction (c1, c2)) ->
    Printf.sprintf "(%s - %s)"
      (combinator_to_string c1)
      (combinator_to_string c2)
  | Primitive (Multiplication (c1, c2)) ->
    Printf.sprintf "(%s * %s)"
      (combinator_to_string c1)
      (combinator_to_string c2)
  | Primitive (Division (c1, c2)) ->
    Printf.sprintf "(%s / %s)"
      (combinator_to_string c1)
      (combinator_to_string c2)
  | IfZero (c1, c2, c3) ->
    Printf.sprintf "(ifzero %s %s %s)"
      (combinator_to_string c1)
      (combinator_to_string c2)
      (combinator_to_string c3)
  | Let (x, c1, c2) ->
    Printf.sprintf "(let %s = %s in %s)"
      x
      (combinator_to_string c1)
      (combinator_to_string c2)

let rec combinator_list_to_string c =
  match c with
  | [] -> ""
  | hd::tl ->
    Printf.sprintf "%s %s"
      (combinator_to_string hd)
      (combinator_list_to_string tl)
