module Id = Core.Id
module Type = Core.Type
module Knorm = K_normalization 

type ident = INT of int | IDENT of Id.t

type t =
  | Unit
  | Int of int
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * ident
  | Sub of Id.t * ident
  | Mul of Id.t * ident
  | Div of Id.t * ident
  | IfEq of Id.t * ident * t * t
  | IfLE of Id.t * ident * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * ident
  | Put of Id.t * ident * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let to_string_ident = function INT i -> string_of_int i | IDENT id -> id

let rec to_string = function
  | Unit -> "()"
  | Int i -> string_of_int i
  | Not x -> Printf.sprintf "not %s" x
  | Neg x -> Printf.sprintf "-(%s)" x
  | Add (x, y) -> Printf.sprintf "%s + %s" x (to_string_ident y)
  | Sub (x, y) -> Printf.sprintf "%s - %s" x (to_string_ident y)
  | Mul (x, y) -> Printf.sprintf "%s * %s" x (to_string_ident y)
  | Div (x, y) -> Printf.sprintf "%s / %s" x (to_string_ident y)
  | Var x -> x
  | IfEq (x, y, e1, e2) ->
      Printf.sprintf "if %s = %s then\n  %s\nelse\n  %s" x (to_string_ident y)
        (to_string e1) (to_string e2)
  | IfLE (x, y, e1, e2) ->
      Printf.sprintf "if %s <= %s then\n  %s\nelse\n  %s" x (to_string_ident y)
        (to_string e1) (to_string e2)
  | Let ((x, type_x), e1, e2) ->
      Printf.sprintf "let %s:%s = %s in\n%s" x (Type.to_string type_x) (to_string e1)  (to_string e2)
  | LetRec (fundef, e) ->
      Printf.sprintf "let rec %s(%s):%s =\n  %s\nin\n%s" (fst fundef.name)
        (String.concat ", " (List.map (fun (name, _) -> name) fundef.args)) (Type.to_string(snd fundef.name ))
        (to_string fundef.body) (to_string e) 
  | App (f, args) -> Printf.sprintf "%s(%s)" f (String.concat ", " args)
  | Tuple vars -> Printf.sprintf "(%s)" (String.concat ", " vars)
  | LetTuple (vars, y, e) ->
      Printf.sprintf "let (%s) = %s in\n%s"
        (String.concat ", " (List.map fst vars))
        y (to_string e)
  | Get (x, y) -> Printf.sprintf "%s.(%s)" x (to_string_ident y)
  | Put (x, y, z) -> Printf.sprintf "%s.(%s) <- %s" x (to_string_ident y) z
  | ExtArray x -> Printf.sprintf "ExtArray(%s)" x
  | ExtFunApp (f, args) ->
      Printf.sprintf "ExtCall %s(%s)" f (String.concat " " args)

let ident_from_id (entry : Id.t) : ident = IDENT entry
let ident_from_int (entry : int) : ident = INT entry

let rec let_build (inside_let : t) (label : Id.t * Type.t) (after_in : t) : t =
  match inside_let with
  | Let (inner_label, inner_t1, inner_t2) ->
      Let (inner_label, inner_t1, let_build inner_t2 label after_in)
  | _ -> Let (label, inside_let, after_in)

let rec net_let_rec (knormal : Knorm.t) : t =
  match knormal with
  | Knorm.Unit -> Unit
  | Knorm.Int i -> Int i
  | Knorm.Not id -> Not id
  | Knorm.Neg id -> Neg id
  | Knorm.Var id -> Var id
  | Knorm.Add (id1, id2) -> Add (id1, ident_from_id id2)
  | Knorm.Sub (id1, id2) -> Sub (id1, ident_from_id id2)
  | Knorm.Mul (id1, id2) -> Mul (id1, ident_from_id id2)
  | Knorm.Div (id1, id2) -> Div (id1, ident_from_id id2)
  | Knorm.IfEq (id1, id2, t1, t2) ->
      IfEq (id1, ident_from_id id2, net_let_rec t1, net_let_rec t2)
  | Knorm.IfLE (id1, id2, t1, t2) ->
      IfLE (id1, ident_from_id id2, net_let_rec t1, net_let_rec t2)
  | Knorm.Let (label, t1, t2) -> (
      let t1_flat = net_let_rec t1 in
      let t2_flat = net_let_rec t2 in
      match t1_flat with
      | Let (inner_label, inner_t1, inner_t2) ->
          Let (inner_label, inner_t1, let_build inner_t2 label t2_flat)
      | _ -> Let (label, t1_flat, t2_flat))
  | Knorm.LetRec ({ Knorm.name; args; body }, t) ->
      let newfundef = { name; args; body = net_let_rec body } in
      LetRec (newfundef, net_let_rec t)
  | Knorm.App (elem, elem_list) -> App (elem, elem_list)
  | Knorm.Tuple list -> Tuple list
  | Knorm.LetTuple (label, id, t) -> LetTuple (label, id, net_let_rec t)
  | Knorm.Get (id1, id2) -> Get (id1, ident_from_id id2)
  | Knorm.Put (id1, id2, id3) -> Put (id1, ident_from_id id2, id3)
  | Knorm.ExtArray id -> ExtArray id
  | Knorm.ExtFunApp (id, id_list) -> ExtFunApp (id, id_list)

let net_let = net_let_rec
