open Fun.Compile
open Fun.Run
open Fun.Parse

let () =
  let fact = parse_from_file "examples/fact.lambda" |> Option.get in
  match compile fact [] |> run with
  | Int x -> Printf.printf "%d\n" x
  | _ -> failwith "Couldn't reach an integer value."
