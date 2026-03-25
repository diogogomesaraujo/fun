(** Module that implements the parsing functions using Menhir's abstractions.*)

(** [parse s] tries to parse a string into a term.*)
let parse s =
  try
    let lexbuf = Lexing.from_string s in
    try
      Some(Parser.prog Lexer.read lexbuf)
      with
      | Parsing.Parse_error ->
        prerr_endline "syntax error";
        None
  with
  | _ ->
    prerr_endline "lexing error";
    None

(** [parse_from_file file_path] tries to parse the content of a file into a term.*)
let parse_from_file file_path =
  let read_lines file_path =
    let contents =
      In_channel.with_open_bin
        file_path
        In_channel.input_all
    in
    String.split_on_char '\n' contents
  in
  let lines = read_lines file_path in
  List.fold_left (fun acc l -> acc ^ l) "" lines |> parse
