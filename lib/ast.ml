(** Module that implements the types that define the exetended λ-Calculus.*)

(** Type that represents a variable name.*)
type identity = string
  [@@deriving show]

(** Type that represents a term recognized by the compiler.*)
type term =
  | Variable of identity
  | Lambda of identity list * term
  | Application of term * term
  | Constant of int
  | IfZero of term * term * term
  | Let of identity * term * term
  | Def of identity * identity list * term * term
  | DefRec of identity * identity list * term * term
  | Addition of term * term
  | Subtraction of term * term
  | Multiplication of term * term
  | Division of term * term
  | Less of term * term
  | Greater of term * term
  | Fix of term
  [@@deriving show]
