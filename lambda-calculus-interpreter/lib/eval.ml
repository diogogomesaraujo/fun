open Ast
open Subst

let rec eval e =
  match e with
  | Constant n -> Constant n
  | Variable _ -> failwith "free variable occurence"
  | Addition (e1, e2) ->
      (match (eval e1,eval e2) with
    |   (Constant e1, Constant e2) -> Constant (e1 + e2)
    |   _ -> failwith "semantic error")
  | Subtraction (e1, e2) ->
      (match (eval e1,eval e2) with
    |   (Constant e1, Constant e2) -> Constant (e1 - e2)
    |   _ -> failwith "semantic error")
  | Multiplication (e1, e2) ->
      (match (eval e1,eval e2) with
    |   (Constant e1, Constant e2) -> Constant (e1 * e2)
    |   _ -> failwith "semantic error")
  | Lambda (x, e1) -> Lambda (x, e1)
  | Application (e1, e2) ->
    let e1' = eval e1 in
    let e2' = eval e2 in
    (match e1' with
    | Lambda (x, e) -> eval (subst e x e2')
    | _ -> failwith "semantic error")
  | IfZero (e1, e2, e3) ->
      (match eval e1 with
      | Constant 0 -> eval e2
      | Constant _ -> eval e3
      | _ -> failwith "semantic error")
  | Fix e1 ->
    (match eval e1 with
      | Lambda (x, e) -> eval (subst e x (Fix e1))
      | _ -> failwith "semantic error")
  | Let (x, e1, e2) -> eval (subst e2 x (eval e1))
