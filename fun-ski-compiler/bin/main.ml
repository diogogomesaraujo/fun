open Fun.Compile
open Fun.Parse
open Fun.Evaluate
open Fun.Flatten
open Fun.Show

let () =
  let fib = parse_from_file "examples/fib.fn" |> Option.get in
  compile fib
  |> flatten
  |> evaluate
  |> combinator_list_to_string
  |> Printf.printf "%s\n"
