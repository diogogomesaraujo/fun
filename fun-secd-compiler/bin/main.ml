open Fun.Compile
open Fun.Run
open Fun.Parse

let () =
  let e = parse_from_file "examples/catalan.fn" |> Option.get in
  match compile e [] |> run with
  | Int x -> Printf.printf "%d\n" x
  | _ -> failwith "Couldn't reach an integer value."
