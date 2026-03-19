open Combinator

let unwind c =
  List.fold_left (
    fun acc c' ->
    match c' with
    | Application (e1, e2) -> acc @ [e1] @ [e2]
    | c' -> acc @ [c']
  ) [] c
