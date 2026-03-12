%{
    open Ast
%}

%token <int> INT
%token <Ast.identity> ID

%token LET
%token ASSIGN
%token IN
%token LPAR
%token RPAR
%token LAMBDA
%token IFZERO
%token FIX
%token ARROW
%token SUM
%token SUB
%token MUL
%token EOF

%left SUM SUB
%left MUL


%start <Ast.term> prog
%%

prog:
  | e = atom; EOF { e }
  ;

atom:
  | i = INT { Constant i }
  | id = ID { Variable id }
  | LPAR; e = term; RPAR {e}
  ;
app:
  | e1 = app; e2 = atom; { Application (e1, e2) }
  | a = atom { a }
  ;

arith:
  | e1 = arith; SUM; e2 = arith { Addition (e1, e2) }
  | e1 = arith; SUB; e2 = arith { Subtraction (e1, e2) }
  | e1 = arith; MUL; e2 = arith { Multiplication (e1, e2) }
  | a = app { a }
  ;

term:
  | LET; x = ID; ASSIGN; e1 = term; IN; e2 = term { Let (x, e1, e2) }
  | LAMBDA; x = ID; ARROW; e = term; { Lambda (x, e) }
  | FIX; e = term; { Fix e }
  | IFZERO; e1 = term; e2 = term; e3 = term; { IfZero (e1, e2, e3) }
  | a = arith { a }
  ;
