module K = K_normalization
module Id = Core.Id
module Type = Core.Type
module M = Map.Make(String)

let find x env =
  try M.find x env
  with Not_found -> x

let rec alpha_rec (env) (t:K.t):K.t =
  match t with
  | K.Unit -> K.Unit
  | K.Int i -> K.Int i
  | K.Var v -> K.Var (find v env)
  | K.Neg x -> K.Neg (alpha_rec env x)
  | K.Add (x, y) -> K.Add (alpha_rec env x, alpha_rec env y)
  | K.Sub (x, y) -> K.Sub (alpha_rec env x, alpha_rec env y)
  | K.App (e, args) -> K.App (alpha_rec env e, List.map (alpha_rec env) args)

  |K.Let ((v, t), e1, e2) ->
      let v' = Id.genid () in
      K.Let ((v', t),
           alpha_rec env e1,
           alpha_rec (M.add v v' env) e2)
  | _ -> failwith "not implemented yet"

let alpha t =
  let search_map = M.empty in
  alpha_rec search_map t
