{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+

rule read =
    parse
    | white { read lexbuf }
    | int { INT (int_of_string (Lexing.lexeme lexbuf))}
    | ":=" { ASSIGN }
    | "." { ARROW }
    | "(" { LPAR }
    | ")" { RPAR }
    | "+" { SUM }
    | "-" { SUB }
    | "*" { MUL }
    | "fun" { LAMBDA }
    | "ifzero" { IFZERO }
    | "fix" { FIX }
    | "let" { LET }
    | "in" { IN }
    | ['a'-'z' 'A'-'Z' '_']+ { ID (Lexing.lexeme lexbuf) }
    | eof { EOF }
