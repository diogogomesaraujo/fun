(** Module that traces every step of the execution.*)

open Execute
open Secd

(** [trace_config c] that constructs a list of the configurations in each step of the execution.*)
let trace_config c =
  let rec trace_rec c l =
    match execute c with
    | (_::[], _, [], _, _) -> l
    | c' ->
      trace_rec (execute c) (l @ [c'])
  in
  trace_rec c []

(** [trace i] receives an instruction list [i] and calls [trace_config] for an initial configuration.*)
let trace i =
  trace_config ([], [], i, [], StoreMap.empty)
