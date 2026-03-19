open Combinator

let rec flatten_rec acc c =
    match c with
    | Application (c1, c2) ->
      acc @ flatten_rec [] c1 @ flatten_rec [] c2
    | c1 -> acc @ [c1]

let flatten c =
  flatten_rec [] c
