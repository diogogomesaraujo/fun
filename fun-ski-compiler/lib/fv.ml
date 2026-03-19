open Lambda

let rec fv x =
  match x with
  | Variable x -> [x]
  | Constant _ -> []
  | Lambda (x, e) -> List.filter (
      fun f -> f <> x
    ) (fv e)
  | Application (e1, e2) -> (fv e1) @ (fv e2)
  | Addition (e1, e2) -> (fv e1) @ (fv e2)
  | Subtraction (e1, e2) -> (fv e1) @ (fv e2)
  | Multiplication (e1, e2) -> (fv e1) @ (fv e2)
  | Fix e -> fv e
  | IfZero (e1, e2, e3) -> (fv e1) @ (fv e2) @ (fv e3)
  | Let (x, e1, e2) -> (fv e1) @ List.filter (
      fun f -> f <> x
    ) (fv e2)

let is_fv v x =
  let fv = fv x in
  match List.find_opt (fun v' -> v = v') fv with
  | Some _ -> false
  | _ -> true
