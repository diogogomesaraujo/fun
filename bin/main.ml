open Fun.Compile
open Fun.Parse
open Fun.Trace
open Fun.Repl
open Fun.Run
open Fun.Exn

let usage_msg = "fun (--repl | <file> [--trace])"

let repl_flag = ref false
let trace_flag = ref false

let input_file = ref ""

let anon_fun filename =
  input_file := filename

let spec_list =
  [("--repl", Arg.Set repl_flag, "Interactive environment");
    ("--trace", Arg.Set repl_flag, "Interactive environment")]

let () =
  Arg.parse spec_list anon_fun usage_msg;

  match !repl_flag = true with
  | true -> repl !trace_flag ();
  | false ->
    try
      let e =
        parse_from_file !input_file
      in

      let prog = compile e [] in

      match prog |> run with
      | Int x ->
        if !trace_flag then Trace.print prog ();
        Printf.printf "< %d\n\n" x; repl_rec !trace_flag ()
      | _ -> raise (Exn (Runtime, "couldn't reach an integer value"))

    with Exn (t, e) -> print_exn (t, e)
