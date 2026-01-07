open Core

type t =
  | Unit
  | Int of int
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec to_string = function
  | Unit -> "()"
  | Int i -> string_of_int i
  | Not x -> Printf.sprintf "not %s" x
  | Neg x -> Printf.sprintf "-(%s)" x
  | Add (x, y) -> Printf.sprintf "%s + %s" x y
  | Sub (x, y) -> Printf.sprintf "%s - %s" x y
  | Var x -> x
  | IfEq (x, y, e1, e2) ->
      Printf.sprintf "if %s = %s then\n  %s\nelse\n  %s" x y (to_string e1)
        (to_string e2)
  | IfLE (x, y, e1, e2) ->
      Printf.sprintf "if %s <= %s then\n  %s\nelse\n  %s" x y (to_string e1)
        (to_string e2)
  | Let ((x, _), e1, e2) ->
      Printf.sprintf "let %s = %s in\n%s" x (to_string e1) (to_string e2)
  | LetRec (fundef, e) ->
      Printf.sprintf "let rec %s(%s) =\n  %s\nin\n%s" (fst fundef.name)
        (String.concat ", " (List.map (fun (name, _) -> name) fundef.args))
        (to_string fundef.body) (to_string e)
  | App (f, args) -> Printf.sprintf "%s(%s)" f (String.concat ", " args)
  | Tuple vars -> Printf.sprintf "(%s)" (String.concat ", " vars)
  | LetTuple (vars, y, e) ->
      Printf.sprintf "let (%s) = %s in\n%s"
        (String.concat ", " (List.map fst vars))
        y (to_string e)
  | Get (x, y) -> Printf.sprintf "%s.(%s)" x y
  | Put (x, y, z) -> Printf.sprintf "%s.(%s) <- %s" x y z
  | ExtArray x -> Printf.sprintf "ExtArray(%s)" x
  | ExtFunApp (f, args) ->
      Printf.sprintf "ExtCall %s(%s)" f (String.concat " " args)

(*Do nothing for the moment need to be modify in the future*)
(*TODO finish the net_let_rec*)
let rec net_let_rec (knormal : K_normalization.t) : t =
  match knormal with
  | Unit -> Unit
  | Int i -> Int i
  | Not id -> Not id
  | Neg id -> Neg id
  | Var id -> Var id
  | Add (id1, id2) -> Add (id1, id2)
  | Sub (id1, id2) -> Sub (id1, id2)
  | IfEq (id1, id2, t1, t2) -> IfEq (id1, id2, net_let_rec t1, net_let_rec t2)
  | IfLE (id1, id2, t1, t2) -> IfLE (id1, id2, net_let_rec t1, net_let_rec t2)
  | Let (label, t1, t2) -> Let (label, net_let_rec t1, net_let_rec t2)
  | LetRec (fundef, t) ->
      let name = fundef.name in
      let args = fundef.args in
      let body = net_let_rec fundef.body in
      let newfundef = { name; args; body } in
      LetRec (newfundef, net_let_rec t)
  | App (elem, elem_list) -> App (elem, elem_list)
  | Tuple list -> Tuple list
  | LetTuple (label, id, t) -> LetTuple (label, id, net_let_rec t)
  | Get (id1, id2) -> Get (id1, id2)
  | Put (id1, id2, id3) -> Put (id1, id2, id3)
  | ExtArray id -> ExtArray id
  | ExtFunApp (id, id_list) -> ExtFunApp (id, id_list)

let net_let (knormal : K_normalization.t) : t = net_let_rec knormal
