(* This code was made following the documentation quoted in the report *)
module Syntax = Core.Syntax
module Type = Core.Type
module Id = Core.Id

exception Type_Error of Syntax.t * Type.t * Type.t

(* Environment for external functions *)
let extenv = ref Id.M.empty

(* Follow the pointers to find the real type *)
let rec deref_typ t =
  match t with
  | Type.Fun (args, ret) -> Type.Fun (List.map deref_typ args, deref_typ ret)
  | Type.Tuple elems -> Type.Tuple (List.map deref_typ elems)
  | Type.Array t -> Type.Array (deref_typ t)
  | Type.Var { contents = None } -> t
  | Type.Var ({ contents = Some t } as r) ->
      let t' = deref_typ t in
      r := Some t';
      t'
  | t -> t

(* Default uninstantiated type variables to Int *)
let rec default_to_int t =
  match t with
  | Type.Fun (args, ret) ->
      Type.Fun (List.map default_to_int args, default_to_int ret)
  | Type.Tuple elems -> Type.Tuple (List.map default_to_int elems)
  | Type.Array t -> Type.Array (default_to_int t)
  | Type.Var ({ contents = None } as r) ->
      Format.eprintf
        "uninstantiated type variable detected; defaulting to int\n";
      r := Some Type.Int;
      Type.Int
  | Type.Var ({ contents = Some t } as r) ->
      let t' = default_to_int t in
      r := Some t';
      t'
  | t -> t

(* Ensure t1 and t2 are the same type *)
let rec unify t1 t2 =
  let t1 = deref_typ t1 in
  let t2 = deref_typ t2 in
  match (t1, t2) with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int -> ()
  | Type.Float, Type.Float -> ()
  | Type.Fun (args1, ret1), Type.Fun (args2, ret2) ->
      if List.length args1 <> List.length args2 then
        raise (Type_Error (Syntax.Unit, t1, t2));
      List.iter2 unify args1 args2;
      unify ret1 ret2
  | Type.Tuple ts1, Type.Tuple ts2 ->
      if List.length ts1 <> List.length ts2 then
        raise (Type_Error (Syntax.Unit, t1, t2));
      List.iter2 unify ts1 ts2
  | Type.Array t1, Type.Array t2 -> unify t1 t2
  | Type.Var r1, Type.Var r2 when r1 == r2 -> ()
  | Type.Var r1, _ -> r1 := Some t2
  | _, Type.Var r2 -> r2 := Some t1
  | _ -> raise (Type_Error (Syntax.Unit, t1, t2))

let rec infer env (e : Syntax.t) =
  match e with
  | Unit -> Type.Unit
  | Bool _ -> Type.Bool
  | Int _ -> Type.Int
  | Not e ->
      unify (infer env e) Type.Bool;
      Type.Bool
  | Neg e ->
      unify (infer env e) Type.Int;
      Type.Int
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
      unify (infer env e1) Type.Int;
      unify (infer env e2) Type.Int;
      Type.Int
  | Eq (e1, e2) | LE (e1, e2) ->
      unify (infer env e1) (infer env e2);
      Type.Bool
  | If (cond, then_body, else_body) ->
      unify (infer env cond) Type.Bool;
      let t_then = infer env then_body in
      let t_else = infer env else_body in
      unify t_then t_else;
      t_then
  | Let ((x, t), e1, e2) ->
      unify t (infer env e1);
      infer (Id.M.add x t env) e2
  | Var x ->
      if Id.M.mem x env then Id.M.find x env
      else if Id.M.mem x !extenv then Id.M.find x !extenv
      else
        (* External function detection *)
        let t = Type.gentyp () in
        extenv := Id.M.add x t !extenv;
        t
  | LetRec ({ name = x, t; args = yts; body = e1 }, e2) ->
      let env_with_f = Id.M.add x t env in
      let env_with_args =
        List.fold_left
          (fun env (arg, arg_type) -> Id.M.add arg arg_type env)
          env_with_f yts
      in
      unify t (Type.Fun (List.map snd yts, infer env_with_args e1));
      infer env_with_f e2
  | App (f, args) ->
      let t_ret = Type.gentyp () in
      unify (infer env f) (Type.Fun (List.map (infer env) args, t_ret));
      t_ret
  | Tuple es -> Type.Tuple (List.map (infer env) es)
  | LetTuple (xts, e1, e2) ->
      unify (Type.Tuple (List.map snd xts)) (infer env e1);
      infer (List.fold_left (fun env (x, t) -> Id.M.add x t env) env xts) e2
  | Array (e1, e2) ->
      unify (infer env e1) Type.Int;
      let t2 = infer env e2 in
      if t2 = Type.Float then failwith "Float Arrays not supported";
      Type.Array t2
  | Get (e1, e2) ->
      let t = Type.gentyp () in
      unify (infer env e1) (Type.Array t);
      unify (infer env e2) Type.Int;
      t
  | Put (e1, e2, e3) ->
      let t = infer env e3 in
      unify (infer env e1) (Type.Array t);
      unify (infer env e2) Type.Int;
      Type.Unit
  | Float _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ ->
      failwith "Float operations are NOT supported"

let f (e : Core.Syntax.t) : Core.Syntax.t =
  extenv := Id.M.empty;

  let t_main = infer Id.M.empty e in
  (try unify Type.Unit t_main with Type_Error _ -> ());

  extenv := Id.M.map default_to_int !extenv;
  ignore (default_to_int (infer Id.M.empty e));
e
  