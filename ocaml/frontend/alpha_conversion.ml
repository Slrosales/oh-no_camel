module K = K_normalization
module Id = Core.Id
module M = Map.Make (String)

let find x env = try M.find x env with Not_found -> x

(* This prevents renaming variables that are already unique vn *)
let is_temp_var s =
  let len = String.length s in
  if len > 1 && s.[0] = 'v' then
    let suffix = String.sub s 1 (len - 1) in
    try
      ignore (int_of_string suffix);
      true
    with _ -> false
  else false

let rec alpha_rec env e =
  match e with
  | K.Unit -> K.Unit
  | K.Int i -> K.Int i
  | K.Neg x -> K.Neg (find x env)
  | K.Add (x, y) -> K.Add (find x env, find y env)
  | K.Sub (x, y) -> K.Sub (find x env, find y env)
  | K.IfEq (x, y, e1, e2) ->
      K.IfEq (find x env, find y env, alpha_rec env e1, alpha_rec env e2)
  | K.IfLE (x, y, e1, e2) ->
      K.IfLE (find x env, find y env, alpha_rec env e1, alpha_rec env e2)
  (* We define a NEW variable v', and map v -> v' for the body e2 *)
  | K.Let ((v, t), e1, e2) ->
      let e1' = alpha_rec env e1 in
      let v', env' =
        if is_temp_var v then (v, env) (* Keep mapping v->v implicitly *)
        else
          let new_v = Id.genid_ v in
          (new_v, M.add v new_v env)
      in
      let e2' = alpha_rec env' e2 in
      K.Let ((v', t), e1', e2')
  | K.Var x -> K.Var (find x env)
  | K.LetRec ({ K.name = x, t; K.args = yts; K.body = e1 }, e2) ->
      let x', env' =
        if is_temp_var x then (x, env)
        else
          let new_x = Id.genid_ x in
          (new_x, M.add x new_x env)
      in
      let env_body, new_args =
        List.fold_right
          (fun (y, t) (env_acc, args_acc) ->
            if is_temp_var y then (env_acc, (y, t) :: args_acc)
            else
              let y' = Id.genid_ y in
              (M.add y y' env_acc, (y', t) :: args_acc))
          yts (env', [])
      in

      let e1' = alpha_rec env_body e1 in
      let e2' = alpha_rec env' e2 in
      K.LetRec ({ K.name = (x', t); K.args = new_args; K.body = e1' }, e2')
  | K.App (x, ys) -> K.App (find x env, List.map (fun y -> find y env) ys)
  | K.ExtFunApp (x, ys) -> K.ExtFunApp (x, List.map (fun y -> find y env) ys)
  | _ -> K.Unit (* IMPLEMENT LATER Tuples/Arrays later *)

let alpha t = alpha_rec M.empty t
