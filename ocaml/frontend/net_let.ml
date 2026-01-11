module Id = Core.Id
module Type = Core.Type

(*TODO complete for float maybe*)

type ident = 
| INT of int
| IDENT of Id.t

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
  | Get (x, y) -> Printf.sprintf "%s.(%s)" x (to_string_ident y)
  | Put (x, y, z) -> Printf.sprintf "%s.(%s) <- %s" x (to_string_ident y) z
  | ExtArray x -> Printf.sprintf "ExtArray(%s)" x
  | ExtFunApp (f, args) ->
      Printf.sprintf "ExtCall %s(%s)" f (String.concat " " args)

let _is_a_let (let_t:t):bool = match let_t with 
| Let _ -> true
| _ -> false

let rec _get_last_after_in (let_t : t) : t =
  match let_t with
  | Let (_, _, Let (label, t1, t2)) -> _get_last_after_in (Let (label, t1, t2))
  | Let (_, _, t3) -> t3
  | _ -> let_t

let rec let_build (inside_let : t) (label : Id.t * Type.t) (after_in : t) : t =
  match inside_let with
  | Let (label_inside, t1_inside, Let (label2, t1, t2)) ->
      Let
        ( label_inside,
          t1_inside,
          let_build (Let (label2, t1, t2)) label after_in )
  | Let (label_inside, t1_inside, t2_inside) ->
      Let (label_inside, t1_inside, Let (label, t2_inside, after_in))
  | _ -> failwith "error let_build take only Let in inside_let"

and flatten (let_tree : t) : t =
  match let_tree with
  | Let (label1, Let (label2, t1, t2), t3) ->
      let flatten_t2 = flatten (Let (label2, t1, t2)) in
      let_build flatten_t2 label1 t3
  | Let (label, t1, t2) -> Let (label, t1, t2)
  | _ -> let_tree

let ident_from_int (entry : int) : ident = INT entry
let ident_from_id (entry : Id.t) : ident = IDENT entry

let rec net_let_rec (knormal : K_normalization.t) : t =
  match knormal with
  | Unit -> Unit
  | Int i -> Int i
  | Not id -> Not id
  | Neg id -> Neg id
  | Var id -> Var id
  | Add (id1, id2) -> Add (id1, ident_from_id id2)
  | Sub (id1, id2) -> Sub (id1, ident_from_id id2)
  | Mul (id1, id2) -> Mul (id1, ident_from_id id2)
  | Div (id1, id2) -> Div (id1, ident_from_id id2)
  | IfEq (id1, id2, t1, t2) ->
      IfEq (id1, ident_from_id id2, net_let_rec t1, net_let_rec t2)
  | IfLE (id1, id2, t1, t2) ->
      IfLE (id1, ident_from_id id2, net_let_rec t1, net_let_rec t2)
  | Let (label, t1, t2) -> flatten (Let (label, net_let_rec t1, net_let_rec t2))
  | LetRec (fundef, t) ->
      let name = fundef.name in
      let args = fundef.args in
      let body = net_let_rec fundef.body in
      let newfundef = { name; args; body } in
      LetRec (newfundef, net_let_rec t)
  | App (elem, elem_list) -> App (elem, elem_list)
  | Tuple list -> Tuple list
  | LetTuple (label, id, t) -> LetTuple (label, id, net_let_rec t)
  | Get (id1, id2) -> Get (id1, ident_from_id id2)
  | Put (id1, id2, id3) -> Put (id1, ident_from_id id2, id3)
  | ExtArray id -> ExtArray id
  | ExtFunApp (id, id_list) -> ExtFunApp (id, id_list)
let net_let (knormal : K_normalization.t) : t = net_let_rec knormal
