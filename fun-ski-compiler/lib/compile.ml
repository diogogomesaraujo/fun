open Fv

let rec compile e =
  match e with
  | Lambda.Constant n -> Combinator.Constant n
  | Lambda.Variable x -> Combinator.Variable x
  | Lambda.Addition (e1, e2) ->
    Combinator.Primitive
      (Combinator.Addition (compile e1, compile e2))
  | Lambda.Subtraction (e1, e2) ->
    Combinator.Primitive
      (Combinator.Subtraction (compile e1, compile e2))
  | Lambda.Multiplication (e1, e2) ->
    Combinator.Primitive
      (Combinator.Multiplication (compile e1, compile e2))
  | Lambda.Application (e1, e2) ->
    Combinator.Application (compile e1, compile e2)
  | Lambda.Lambda (x, Lambda.Variable v) when x = v -> Combinator.I
  | Lambda.Lambda (x, p) when is_fv x p = false ->
    Combinator.Application (Combinator.K, compile p)
  | Lambda.Lambda (x, Lambda.Application (p, Lambda.Variable x'))
    when x = x' && is_fv x p -> compile p
  | Lambda.Lambda (x, Lambda.Application (p , q)) when is_fv x p = false ->
    Combinator.Application
      (Combinator.B,
        Combinator.Application
        (compile p, compile (Lambda.Lambda (x, q))))
  | Lambda.Lambda (x, Lambda.Application (p , q)) when is_fv x q = false ->
    Combinator.Application
      (Combinator.C, Combinator.Application
        (compile (Lambda.Lambda (x, p)), compile q))
  | Lambda.Lambda (x, Lambda.Application (p , q)) when is_fv x p && is_fv x q ->
    Combinator.Application
      (Combinator.S, Combinator.Application
        (compile (Lambda.Lambda (x, p)),
          compile (Lambda.Lambda (x, q))))
  | Lambda.Lambda _ ->
    failwith "invalid lambda term"
  | Lambda.Fix e -> Combinator.Y (compile e)
  | Lambda.IfZero (e1, e2, e3) ->
    Combinator.IfZero (compile e1, compile e2, compile e3)
  | Lambda.Let (x, e1, e2) ->
    Combinator.Let (x, compile e1, compile e2)
