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
%token REC
%token DOTS
%token THEN
%token ELSE
%token LESS
%token GREATER
%token ASSIGN
%token DIFF

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
  | e1 = mul; MUL; e2 = cond { Multiplication (e1, e2) }
  | e1 = mul; DIV; e2 = cond { Division (e1, e2) }
  | c = cond { c }

cond:
  | c1 = app; LESS; c2 = app { Less(c1, c2) }
  | c1 = app; GREATER; c2 = app { Greater(c1, c2) }
  | c1 = app; EQUAL; c2 = app { Equal(c1, c2) }
  | c1 = app; LESS; EQUAL; c2 = app { LessEqual(c1, c2) }
  | c1 = app; GREATER; EQUAL; c2 = app { GreaterEqual(c1, c2) }
  | c1 = app; DIFF; c2 = app { Different(c1, c2) }
  | a = app { a }
  ;

app:
  | a1 = app; a2 = atomic { Application (a1, a2) }
  | a = atomic { a }

expr:
  | s = sum { s }
  | DEF; REC; id = ID; DOTS; vars = list(ID); ASSIGN; e1 = expr; IN e2 = expr { DefRec (id, vars, e1, e2) }
  | DEF; id = ID; DOTS; vars = list(ID); ASSIGN; e1 = expr; IN e2 = expr { Def (id, vars, e1, e2) }
  | FUN; idl = list(ID); ARROW; e = expr { Lambda (idl, e) }
  | LET; id = ID; ASSIGN; e1 = expr; IN e2 = expr { Let (id, e1, e2) }
  | IFZERO; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { IfZero (e1, e2, e3) }
  | FIX; a = atomic { Fix (a) }
