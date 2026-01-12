module Id = Core.Id
module Type = Core.Type

(*TODO higher-order function under closure implementation*)

module SetString = Set.Make(String)

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
  | App of Id.t * Id.t list (*note we have now apply_direct(l, (x1 … xn))*)
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * ident
  | Put of Id.t * ident * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
 

and fundef = { name : Id.t; args : Id.t list; body : t }
(*rename label by adding _*)

type all_fun = Fun of fundef * all_fun | Main of t
type toplevel = all_fun

let fun_list:(Net_let.fundef) list = [] 
let to_string_ident = function INT i -> string_of_int i | IDENT id -> id

let rec to_string_t = function
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
        (to_string_t e1) (to_string_t e2)
  | IfLE (x, y, e1, e2) ->
      Printf.sprintf "if %s <= %s then\n  %s\nelse\n  %s" x (to_string_ident y)
        (to_string_t e1) (to_string_t e2)
  | Let ((x, _), e1, e2) ->
      Printf.sprintf "let %s = %s in\n%s" x (to_string_t e1) (to_string_t e2)
  | App (f, args) ->
      Printf.sprintf "apply_direct(%s(%s))" f (String.concat ", " args)
  | Tuple vars -> Printf.sprintf "(%s)" (String.concat ", " vars)
  | LetTuple (vars, y, e) ->
      Printf.sprintf "let (%s) = %s in\n%s"
        (String.concat ", " (List.map fst vars))
        y (to_string_t e)
  | Get (x, y) -> Printf.sprintf "%s.(%s)" x (to_string_ident y)
  | Put (x, y, z) -> Printf.sprintf "%s.(%s) <- %s" x (to_string_ident y) z
  | ExtArray x -> Printf.sprintf "ExtArray(%s)" x
  | ExtFunApp (f, args) ->
      Printf.sprintf "ExtCall %s(%s)" f (String.concat " " args)

let rec get_name_arg x =
  match x with
  | [] -> ""
  | name :: [] -> name
  | name :: tail -> name ^ " " ^ get_name_arg tail

let rec to_string_all_fun (all_fun : all_fun) : string =
  match all_fun with
  | Main t -> to_string_t t
  | Fun (fundef, e) ->
      let name = fundef.name in
      let arg = fundef.args in
      let param = get_name_arg arg in
      let body = fundef.body in
      Printf.sprintf "label: %s\nparameters: %s\ncode:\n\t%s\n\n__end__\n%s"
        name param (to_string_t body) (to_string_all_fun e)

let to_string (toplevel : toplevel) : string = to_string_all_fun toplevel

let ident_conversion (entry : Net_let.ident) : ident =
  match entry with IDENT id -> IDENT id | INT i -> INT i

let remove_type elem = match elem with
| (name,_) -> name 

let rec closure_t_rec (net_let : Net_let.t) (closure:SetString.t): t*fundef list =
  match net_let with
  | Unit -> (Unit,[])
  | Int i -> (Int i,[]) 
  | Not id -> (Not id,[])
  | Neg id -> (Neg id,[])
  | Var id -> (Var id,[])
  | Add (id1, id2) -> (Add (id1, ident_conversion id2),[])
  | Sub (id1, id2) -> (Sub (id1, ident_conversion id2),[])
  | Mul (id1, id2) -> (Mul (id1, ident_conversion id2),[])
  | Div (id1, id2) -> (Div (id1, ident_conversion id2),[])
  | IfEq (id1, id2, t1, t2) ->
      let new_t1,then_new_fun = closure_t_rec t1 closure in
      let new_t2,else_new_fun = closure_t_rec t2 closure in
      (IfEq (id1, ident_conversion id2, new_t1 , new_t2),then_new_fun@else_new_fun)
  | IfLE (id1, id2, t1, t2) ->
      let new_t1,then_new_fun = closure_t_rec t1 closure in
      let new_t2,else_new_fun = closure_t_rec t2 closure in
      (IfLE (id1, ident_conversion id2, new_t1 , new_t2),then_new_fun@else_new_fun)
  | Let (label, t1, t2) -> 
      let new_t1,new_fun_be_in = closure_t_rec t1 closure in 
      let new_t2,new_fun_aff_in = closure_t_rec t2 closure in
      (Let (label, new_t1, new_t2),new_fun_be_in@new_fun_aff_in)
  | App (label, elem_list) -> 
    let new_label = "_"^label in
    (App (new_label, elem_list),[])
  | Tuple list -> (Tuple list,[])
  | LetTuple (label, id, t) -> 
    let new_t,new_fun = closure_t_rec t closure in
    (LetTuple (label, id, new_t),new_fun)
  | Get (id1, id2) -> (Get (id1, ident_conversion id2),[])
  | Put (id1, id2, id3) -> (Put (id1, ident_conversion id2, id3),[])
  | ExtArray id -> (ExtArray id,[])
  | ExtFunApp (id, id_list) -> (ExtFunApp (id, id_list),[])
  | LetRec (fundef,t) ->
    let (name,_) = fundef.name in
    let args = List.map remove_type fundef.args in
    let new_body,fun_to_add_from_body =closure_t_rec fundef.body closure in 
    let return,fun_to_add = closure_t_rec t closure in 
    (return, fun_to_add_from_body @[{name=name;args=args;body=new_body}]@fun_to_add)

and closure_fun_rec(net_let : Net_let.t) (closure:SetString.t) : (all_fun*fundef list) =
  match net_let with
  | Net_let.LetRec (fundef,e) -> 
    let (name,_) = fundef.name in
    let args = List.map remove_type fundef.args in
    let new_body,fun_to_add_from_body =closure_t_rec fundef.body closure in 
    let return,fun_to_add = closure_fun_rec e closure in
    (return, fun_to_add_from_body @[{name=name;args=args;body=new_body}]@fun_to_add)
  | other -> 
    let main,_fun_to_add = closure_t_rec other closure in
    (Main (main),[])

let rec add_fun (fun_list:fundef list)(main:all_fun):all_fun = match fun_list with
| [] -> main
| e::tail -> Fun(e,add_fun tail main) 
let closure_fun (net_let : Net_let.t) (closure:SetString.t):all_fun = let fun_closure,fun_to_add = closure_fun_rec net_let closure in
let ret = (add_fun fun_to_add fun_closure) in
ret
let closure (net_let : Net_let.t)  : toplevel = closure_fun net_let SetString.empty
