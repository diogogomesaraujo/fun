type identity = string
  [@@deriving show]

type op =
  | Addition of comb * comb
  | Multiplication of comb * comb
  | Subtraction of comb * comb
  [@@deriving show]

and comb =
  | Variable of identity
  | Application of comb * comb
  | S | K | I
  | B | C
  | Y of comb
  | Constant of int
  | Primitive of op
  | IfZero of comb * comb * comb
  | Let of identity * comb * comb
  [@@deriving show]
