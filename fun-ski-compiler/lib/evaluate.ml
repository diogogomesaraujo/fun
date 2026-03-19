open Normal
open Rewrite
open Unwind

let rec evaluate c =
  match is_normal_form c with
  | true -> c
  | false ->
    let c1 = unwind c in
    evaluate (rewrite c1)
