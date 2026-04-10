%{
    open Ast
%}

%token <int> INT
%token <string> ID

%token EQUAL
%token IN
%token LET
%token IFZERO
%token FIX
%token LPAR
%token RPAR
%token SUM
%token SUB
%token MUL
%token DIV
%token FUN
%token ARROW
%token EOF
%token DEF
%token DEFREC
%token DOTS

%start <Ast.term> prog
%%

prog:
  | e = expr; EOF { e }
  ;

atomic:
  | i = INT { Constant i }
  | id = ID { Variable id }
  | LPAR; e = expr; RPAR { e }
  ;

sum:
  | e1 = sum; SUM; e2 = mul { Addition (e1, e2) }
  | e1 = sum; SUB; e2 = mul { Subtraction (e1, e2) }
  | m = mul { m }
  ;

mul:
  | e1 = mul; MUL; e2 = app { Multiplication (e1, e2) }
  | e1 = mul; DIV; e2 = app { Division (e1, e2) }
  | a = app { a }

app:
  | a1 = app; a2 = atomic { Application (a1, a2) }
  | a = atomic { a }

expr:
  | a = sum { a }
  | FUN; idl = list(ID); ARROW; e = expr { Lambda (idl, e) }
  | LET; id = ID; EQUAL; e1 = expr; IN e2 = expr { Let (id, e1, e2) }
  | DEF; id = ID; DOTS; vars = list(ID); EQUAL; e1 = expr; IN e2 = expr { Def (id, vars, e1, e2) }
  | DEFREC; id = ID; DOTS; vars = list(ID); EQUAL; e1 = expr; IN e2 = expr { DefRec (id, vars, e1, e2) }
  | IFZERO; a1 = atomic; a2 = atomic; a3 = atomic { IfZero (a1, a2, a3) }
  | FIX; a = atomic { Fix (a) }
