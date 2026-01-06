open Core
(*
in this comment are the element for research tree of a string
module SetString = Set.Make(String)

let function_name = SetString.empty
*)
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
  | App of Id.t * Id.t list (*note we have now apply_direct(l, (x1 … xn))*)
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list

type fundef = { name : Id.t ; args : Id.t list; body : t } (*rename label by adding _*)

type all_fun =
| Fun of fundef * all_fun
| Main of t

type toplevel = all_fun
let rec to_string_t = function
  | Unit -> "()"
  | Int i -> string_of_int i
  | Not x -> Printf.sprintf "not %s" x
  | Neg x -> Printf.sprintf "-(%s)" x
  | Add (x, y) -> Printf.sprintf "%s + %s" x y
  | Sub (x, y) -> Printf.sprintf "%s - %s" x y
  | Var x -> x
  | IfEq (x, y, e1, e2) ->
      Printf.sprintf "if %s = %s then\n  %s\nelse\n  %s" 
        x y (to_string_t e1) (to_string_t e2)
  | IfLE (x, y, e1, e2) ->
      Printf.sprintf "if %s <= %s then\n  %s\nelse\n  %s" 
        x y (to_string_t e1) (to_string_t e2)
  | Let ((x, _), e1, e2) -> 
      Printf.sprintf "let %s = %s in\n%s" x (to_string_t e1) (to_string_t e2)
  | App (f, args) ->
      Printf.sprintf "apply_direct(%s(%s))" f (String.concat ", " args)
  | Tuple vars -> 
      Printf.sprintf "(%s)" (String.concat ", " vars)
  | LetTuple (vars, y, e) -> 
      Printf.sprintf "let (%s) = %s in\n%s" 
        (String.concat ", " (List.map fst vars)) y (to_string_t e)
  | Get (x, y) -> Printf.sprintf "%s.(%s)" x y
  | Put (x, y, z) -> Printf.sprintf "%s.(%s) <- %s" x y z
  | ExtArray x -> Printf.sprintf "ExtArray(%s)" x
  | ExtFunApp (f, args) -> Printf.sprintf "ExtCall %s(%s)" f (String.concat " " args)

let rec get_name_arg x = match x with
      | [] -> ""
      |  name ::[] -> name 
      |  name::tail -> name ^ " " ^ get_name_arg tail

let rec to_string_all_fun (all_fun:all_fun):string = match all_fun with
| Main t -> to_string_t t
| Fun (fundef, e) -> 
      let name = fundef.name in
      let arg = fundef.args in 
      let param = get_name_arg arg in
      let body = fundef.body in
      Printf.sprintf "label: %s\nparameters: %s\ncode:\n\t%s\n\n__end__\n%s" name param (to_string_t body) (to_string_all_fun e)

let to_string (toplevel:toplevel):string = to_string_all_fun toplevel

let rec closure_t_rec (net_let:Net_let.t):t = match net_let with
| Unit -> Unit
| Int i  -> Int i
| Not id -> Not id
| Neg id -> Neg id
| Var ( id) -> Var id
| Add (id1 , id2 ) -> Add(id1,id2)
| Sub (id1,id2) -> Sub (id1,id2) 
| IfEq (id1 , id2 , t1 , t2) -> IfEq(id1,id2,closure_t_rec t1,closure_t_rec t2) 
| IfLE (id1 , id2 , t1 , t2) -> IfLE(id1,id2,closure_t_rec t1,closure_t_rec t2)
| Let (label , t1 , t2) -> Let (label,closure_t_rec t1,closure_t_rec t2)

| LetRec _ -> failwith "not suppose to get here"

| App (elem , elem_list )-> App(elem,elem_list)
| Tuple (list) -> Tuple (list)
| LetTuple (label ,id , t) -> LetTuple (label,id ,closure_t_rec t)
| Get (id1,id2) -> Get (id1,id2)
| Put (id1,id2,id3) -> Put (id1,id2,id3)
| ExtArray id -> ExtArray(id)
| ExtFunApp (id , id_list) -> ExtFunApp(id,id_list)
let closure_fun_rec (net_let:Net_let.t):all_fun = match net_let with
  | LetRec _ -> failwith "to implement closure"
  | remain -> Main (closure_t_rec remain)

let closure (net_let:Net_let.t):toplevel = closure_fun_rec net_let
