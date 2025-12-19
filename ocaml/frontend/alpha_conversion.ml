open Core.Syntax
open K_normalization

module Type = Core.Type
module M = Map.Make(String)

let find x env =
  try M.find x env
  with Not_found -> x

let rec alpha_rec env t =
  match t with
  | Unit -> Unit
  | Int i -> Int i
  | Float f -> Float f
  | Var v -> Var (find v env)
  | Neg x -> Neg (alpha_rec env x)
  | Add (x, y) -> Add (alpha_rec env x, alpha_rec env y)
  | Sub (x, y) -> Sub (alpha_rec env x, alpha_rec env y)
  | FNeg x -> FNeg (alpha_rec env x)
  | FAdd (x, y) -> FAdd (alpha_rec env x, alpha_rec env y)
  | FSub (x, y) -> FSub (alpha_rec env x, alpha_rec env y)
  | FMul (x, y) -> FMul (alpha_rec env x, alpha_rec env y)
  | FDiv (x, y) -> FDiv (alpha_rec env x, alpha_rec env y)
  | Let ((v, t), e1, e2) ->
      let v' = Id.genid () in
      Let ((v', t),
           alpha_rec env e1,
           alpha_rec (M.add v v' env) e2)
  | _ -> failwith "not implemented yet"

let alpha t =
  let search_map = M.empty in
  alpha_rec search_map t
