open Fun.Ast
open Fun.Compile
open Fun.Run

let () =
  let fact = Let (
    "fact",
    Fix (Lambda ("g", Lambda ("x", IfZero (Variable "x", Constant 1, Multiplication (Variable "x", Application (Variable "g", Subtraction (Variable "x", Constant 1))))))),
    Application (Variable "fact", Constant 5)) in
  match compile fact [] |> run with
  | Int x -> Printf.printf "%d\n" x
  | _ -> failwith "Couldn't reach an integer value."
