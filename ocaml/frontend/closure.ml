module Id = Core.Id
module Type = Core.Type
module Env_closure = Env_closure 


module S = Set.Make(String)

type fonction_table = (string , string) Hashtbl.t

(*
-------------------------------------------
      Type
-------------------------------------------
*)

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
  | Let of Id.t * t * t
  | Var of Id.t
  | New of ident
  | App of Id.t * Id.t list (*note we have now apply_direct(l, (x1 … xn))*)
  | AppClo of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of Id.t  list * Id.t * t
  | Get of Id.t * ident
  | Put of Id.t * ident * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
 

and fundef = { name : Id.t; args : Id.t list; body : t }
(*rename label by adding _*)

type all_fun = Fun of fundef * all_fun | Main of t
type toplevel = all_fun




(*
----------------------------------------
    To String
----------------------------------------
*)
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
  | Var x -> "Var "^x
  | IfEq (x, y, e1, e2) ->
      Printf.sprintf "if %s = %s then\n  %s\nelse\n  %s" x (to_string_ident y)
        (to_string_t e1) (to_string_t e2)
  | IfLE (x, y, e1, e2) ->
      Printf.sprintf "if %s <= %s then\n  %s\nelse\n  %s" x (to_string_ident y)
        (to_string_t e1) (to_string_t e2)
  | Let ((x), e1, e2) ->
      Printf.sprintf "let %s = %s in\n%s" x (to_string_t e1) (to_string_t e2)
  | App (f, args) ->
      Printf.sprintf "apply_direct(%s(%s))" f (String.concat ", " args)
  | Tuple vars -> Printf.sprintf "(%s)" (String.concat ", " vars)
  | LetTuple (vars, y, e) ->
      Printf.sprintf "let (%s) = %s in\n%s"
        (String.concat ", " vars)
        y (to_string_t e)
  | Get (x, y) -> Printf.sprintf "mem ( %s + %s )" x (to_string_ident y)
  | Put (x, y, z) -> Printf.sprintf "mem ( %s + %s ) <- %s" x (to_string_ident y) z
  | ExtArray x -> Printf.sprintf "ExtArray(%s)" x
  | ExtFunApp (f, args) ->
      Printf.sprintf "ExtCall %s(%s)" f (String.concat " " args)
  | AppClo (f, args) ->
      Printf.sprintf "apply_closure(%s(%s))" f (String.concat ", " args)
  | New i -> ("New "^to_string_ident i)

let rec get_name_arg x =
  match x with
  | [] -> ""
  | (name) :: [] -> name
  | (name) :: tail -> name ^ " " ^ get_name_arg tail

let rec to_string_all_fun (all_fun : all_fun) : string =
  match all_fun with
  | Main t -> to_string_t t
  | Fun (fundef, e) ->
      let name = fundef.name in
      let arg = fundef.args in
      let param = get_name_arg arg in
      let body = fundef.body in
      Printf.sprintf "label: %s \nparameters: %s\ncode:\n\n%s\n\n__end__\n\n%s"
        name  param (to_string_t body) (to_string_all_fun e)

let to_string (toplevel : toplevel) : string = to_string_all_fun toplevel

(*----------------------------------------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------------------------------------------
*)

let remove_type elem = match elem with
| (name,_) -> name 

let rec remove_var (type_var:Type.t):Type.t = (match type_var with
                                    | Var t -> remove_var (match !t with
                                    | None -> Var t
                                    | Some t -> remove_var t) 
                                    | _ -> type_var       ) 


let rec add_fun (fun_list:fundef list)(main:all_fun):all_fun = match fun_list with
| [] -> main
| e::tail -> Fun(e,add_fun tail main) 

let union (l1:Id.t list) (l2:Id.t list):string list =
  S.elements (S.union (S.of_list l1) (S.of_list l2))

let rec pre_turn_true (id_list:Id.t list)(context:Id.t)(env:Env_closure.env)(already_added:(string,bool) Hashtbl.t):unit = 
  match id_list with
  | []-> ()
  | e::tail -> (match Env_closure.get_var_in_env ~label_fun:context ~id:e ~env:env with
                | None -> pre_turn_true tail context env already_added
                | Some _ -> Hashtbl.replace already_added e true  ;
                            pre_turn_true tail context env already_added)

let rec add_var_to_table (list_var:Id.t list)(table :(string,bool) Hashtbl.t):unit =
  match list_var with
  | [] -> ()
  | e::tail -> Hashtbl.add table e false ; add_var_to_table tail table

let create_hastbl (fundef:Id.t)(env:Env_closure.env) :(string,bool) Hashtbl.t=
    let size_env =match Env_closure.MapEnv.find_opt fundef env with 
    | None -> 0
    | Some s -> Env_closure.MapVar.cardinal s in
    let name_var_fundef = Env_closure.get_var_from ~label_fun:fundef ~env:env in
    let hestable :(string,bool) Hashtbl.t= Hashtbl.create size_env in
    add_var_to_table name_var_fundef hestable ;
    hestable
let add_reading_ident (ident:ident):Id.t option =
  match ident with
  | INT _ -> None
  | IDENT id -> Some id

let rec add_let_closure (id:(Id.t) list)(t:t)(contexte:Id.t)(env:Env_closure.env)(already_added:(string,bool) Hashtbl.t):t=
  match id with
  | [] -> (t)
  | (name)::tail ->( 
              let is_in_table = match Hashtbl.find_opt already_added name with
              | None -> false 
              | Some s -> s in

              match Env_closure.get_var_in_env ~label_fun:contexte ~id:name ~env:env with
              | None -> add_let_closure tail t contexte  env already_added
              | Some  _ when is_in_table ->  add_let_closure tail t contexte env already_added
              | Some i -> 
                 Hashtbl.replace already_added name true;
        
                (Let((name),Get("%self",INT i),(add_let_closure tail t contexte env already_added))))



let rec get_id_let (t:t):Id.t list =
  match t with
  | Unit -> []
  | Int _ -> []
  | Not id -> [id]
  | Neg id -> [id]
  | Var id -> [ id]
  | Add (id,ident)| Sub (id,ident) | Mul (id,ident) | Div (id,ident)  ->(

  match add_reading_ident ident with
    | None -> [id]
    | Some id2 -> [id;id2;])
  | IfEq (id,ident,t1,t2) |IfLE (id,ident,t1,t2) -> 
    let id_t1 = get_id_let t1 in
    let id_t2 = get_id_let t2 in
    let union_t1_t2 = union id_t1 id_t2 in
    ( match add_reading_ident ident with
    | None -> union [id] union_t1_t2
    | Some id2 -> union [id;id2] union_t1_t2) 
  | Let _ -> []
  | App (_,id_list) -> id_list
  | Tuple (id_list) -> id_list (*TODO when tuple added*)
  | LetTuple (_id_list,_id,_) -> [] (*TODO when letTuple*) 
  | Get (id,ident) -> ( match add_reading_ident ident with
    | None -> [id]
    | Some id2 -> [id;id2])
  | Put (id1,ident,id2) -> ( match add_reading_ident ident with
    | None -> [id1;id2]
    | Some id3 -> [id1;id2;id3])
  | ExtArray (id) -> [id]
  | ExtFunApp (_,id_list) -> id_list
  | AppClo (_,id_list) -> id_list
  | New (ident) ->( match ident with
  | INT _ ->  []
  | IDENT id -> [id])

let print_hashtable (tbl : fonction_table) : unit =
  Hashtbl.iter (fun key value ->
    Printf.printf "%s -> %s\n" key value
  ) tbl

let rec add_closure_variable (fundef:Id.t)(var_closure:Id.t list)(env:Env_closure.env)(context:Id.t)(already_added:(string,bool) Hashtbl.t)(t:t)=
 match var_closure with
 | [] -> t
 | e::tail ->
  let (var)= match Env_closure.get_var_in_env ~label_fun:fundef ~id:e ~env:env  with
  | None -> failwith ""
  | Some s -> s  in
  add_let_closure [e] (Let ((fundef^"."^string_of_int (var)) , Put(fundef,INT var,e),add_closure_variable fundef tail env context already_added t)) (context) env already_added

let initialise_closure (var_closure_name:Id.t)(env:Env_closure.env)(context:Id.t)(already_added:(string,bool) Hashtbl.t)(t:t):t=
    let var_closure_list = Env_closure.get_var_from ~label_fun:var_closure_name ~env:env in
    Let ((var_closure_name),New (INT ((List.length var_closure_list)*4+4  )),
    Let (("addr_"^var_closure_name),Var ("_"^var_closure_name),
    Let ((var_closure_name^".0"),Put (var_closure_name,INT 0,"addr_"^var_closure_name  ),add_closure_variable var_closure_name var_closure_list env context already_added t)) )

let initialise_closure_smart (var_closure_name:Id.t)(env:Env_closure.env)(context:Id.t)(already_added:(string,bool) Hashtbl.t)(t:t):t =
  match Env_closure.MapEnv.exists (fun x _not_use -> if x=var_closure_name then true else false ) env  with
    | true ->  initialise_closure var_closure_name env context (already_added:(string,bool) Hashtbl.t)(t)
    | false -> t

let rec initialise_closure_smart_list (var_closure_name:Id.t list)(env:Env_closure.env)(context:Id.t)(already_added:(string,bool) Hashtbl.t)(t:t):t =
  match var_closure_name with
  | [] -> t
  | e::tail -> initialise_closure_smart e env context already_added (initialise_closure_smart_list tail env context already_added t)

(*------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------
*)

let ident_conversion (entry : Net_let.ident)(context:string)(map : Env_closure.closure)(env:Env_closure.env) : ident*Env_closure.env =
  match entry with IDENT id ->
                                (
                                  match Env_closure.need_closure_bool id context map with 
                                | true -> let new_env = Env_closure.add_var_env ~label_fun:context ~ident:(id) ~env:env in
                                  (IDENT (id),new_env)
                                | false -> (IDENT (id),env) )
    | INT i -> (INT i,env)



            
                                  
let rec closure_t_rec (net_let : Net_let.t)(context:string) (closure:Env_closure.closure)(map_type:Env_closure.var_type) (env:Env_closure.env) : t*fundef list*Env_closure.env =
  match net_let with
  | Unit -> (Unit,[],env)
  | Int i -> (Int i,[],env) 
  | Not id ->(
    
     match Env_closure.need_closure_bool id context closure with
     | false -> (Not id,[],env)
      | true -> (Not id,[],(Env_closure.add_var_env ~label_fun:context ~ident:(id) ~env:env)))
  | Neg id -> (
    
     match Env_closure.need_closure_bool id context closure with
     | false -> (Neg id,[],env)
      | true -> (Neg id,[],(Env_closure.add_var_env ~label_fun:context ~ident:(id) ~env:env)))
  | Var id ->  (
    
     match Env_closure.need_closure_bool id context closure with
     | false -> (Var id,[],env)
     | true -> (Var id,[],(Env_closure.add_var_env ~label_fun:context ~ident:(id) ~env:env)))
  | Add (id1, id2) -> (

    let env_id1 = 
      match Env_closure.need_closure_bool id1 context closure with
    | false -> env 
    | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id1) ~env:env in
    let new_id2,env_id2 = ident_conversion id2 context closure env_id1 in
   (Add (id1, new_id2),[],env_id2)
              )
  | Sub (id1, id2) -> 
    (
    let env_id1 = 
      match Env_closure.need_closure_bool id1 context closure with
    | false -> env 
    | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id1) ~env:env in
    let new_id2,env_id2 = ident_conversion id2 context closure env_id1 in
   (Sub (id1, new_id2),[],env_id2)
              )
              
  | Mul (id1, id2) -> 
    (
    let env_id1 = 
      match Env_closure.need_closure_bool id1 context closure with
    | false -> env 
    | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id1) ~env:env in
    let new_id2,env_id2 = ident_conversion id2 context closure env_id1 in
   (Mul (id1, new_id2),[],env_id2)
              )
              
  | Div (id1, id2) -> 
    (
    let env_id1 = 
      match Env_closure.need_closure_bool id1 context closure with
    | false -> env 
    | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id1) ~env:env in
    let new_id2,env_id2 = ident_conversion id2 context closure env_id1 in
   (Div(id1, new_id2),[],env_id2)
              )
  
  | IfEq (id1, id2, t1, t2) ->
      let env_id1 = match Env_closure.need_closure_bool id1 context closure with
      | false -> env
      | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id1) ~env:env in
      let new_id2,env_id2 = ident_conversion id2 context  closure env_id1 in

    let new_t1,then_new_fun,t1_env = closure_t_rec t1 context closure map_type env_id2  in
      let new_t2,else_new_fun,t2_env = closure_t_rec t2 context closure map_type t1_env  in
      (IfEq (id1, new_id2 , new_t1 , new_t2),then_new_fun@else_new_fun,t2_env)
              
      
  | IfLE (id1, id2, t1, t2) ->
      let env_id1 = match Env_closure.need_closure_bool id1 context closure with
      | false -> env
      | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id1) ~env:env in
      let new_id2,env_id2 = ident_conversion id2 context closure  env_id1 in

    let new_t1,then_new_fun,t1_env = closure_t_rec t1 context closure map_type env_id2 in
      let new_t2,else_new_fun,t2_env = closure_t_rec t2 context closure map_type t1_env  in
      (IfLE (id1, new_id2 , new_t1 , new_t2),then_new_fun@else_new_fun,t2_env)

  | Let ((name,type_name), t1, t2) -> 
      
      let type_name = match t1 with
      | App((label),_)  -> (match Env_closure.get_type  map_type label with
                          | None -> type_name
                          | Some type_label -> (
                                    
                                    match remove_var type_label with
                                            |  Fun (_,ret) -> ret
                                            | _ -> type_label) )


      | _ -> type_name in
      let new_map = Env_closure.add_type map_type name type_name in
      let new_t1,new_fun_be_in,env_t1 = closure_t_rec t1 context closure map_type env   in 
      let new_closure = Env_closure.add_element name context closure in
      let new_t2,new_fun_aff_in,env_t2 = closure_t_rec t2 context new_closure new_map env_t1  in
      (Let ((name), new_t1, new_t2),new_fun_be_in@new_fun_aff_in,env_t2)
  | App (label, elem_list) -> (

    let new_label =label
  in
    let var_to_add = Env_closure.list_need_closure elem_list context map_type closure in
    let new_env = Env_closure.add_var_env_from_list ~label_fun:context ~id_list:var_to_add ~env:env in
   (App (new_label, elem_list),[],new_env))
  | Tuple list -> 

    let var_to_add =Env_closure.list_need_closure list context map_type closure in
    let new_env = Env_closure.add_var_env_from_list ~label_fun:context ~id_list:var_to_add ~env:env in
    (Tuple list,[],new_env)
  | LetTuple (label, id, t) -> 
    let label_without_list = List.map (fun (x,_)-> x) label in
    let new_closure = Env_closure.add_element_list label_without_list context closure in
    let var_to_add =Env_closure.list_need_closure label_without_list context map_type new_closure in
    let new_env = Env_closure.add_var_env_from_list ~label_fun:context ~id_list:var_to_add ~env:env in
    let new_env =
    match Env_closure.need_closure_bool id context new_closure with
    | false -> new_env
    | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id) ~env:new_env 
    in
    let new_t,new_fun,env = closure_t_rec t context new_closure map_type new_env  in
    (LetTuple (label_without_list, id, new_t),new_fun,env)
  | Get (id1, id2) -> 
    let env_id1 =match Env_closure.need_closure_bool id1 context closure with
    | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id1) ~env:env 
    | false -> env in
    let new_id2,env_id2 =ident_conversion id2 context closure env_id1 in
    (Get (id1, new_id2 ),[],env_id2)
  | Put (id1, id2, id3) -> 
    let env_id1 =match Env_closure.need_closure_bool id1 context closure with
    | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id1) ~env:env 
    | false -> env in
    let new_id2,env_id2 =ident_conversion id2 context closure env_id1 in
    let env_id3 =match Env_closure.need_closure_bool id3 context closure with
    | true -> Env_closure.add_var_env ~label_fun:context ~ident:(id3) ~env:env_id2 
    | false -> env_id2 in
    (Put (id1, new_id2 , id3),[],env_id3)
  | ExtArray id -> 
    (*Call ?*)
    (ExtArray id,[],env)
  
    | ExtFunApp (id, id_list) -> 


    let add_id_list =Env_closure.list_need_closure id_list context map_type closure in
      let new_env = Env_closure.add_var_env_from_list ~label_fun:context ~id_list:add_id_list ~env:env in
    (ExtFunApp (id, id_list),[],new_env)
  
    | LetRec (fundef,t) ->
    let (name,_) = fundef.name in
    let args_without_type = List.map (remove_type) fundef.args in

    let map_letrec = Env_closure.add_arg args_without_type name closure in 

    let new_body,fun_to_add_from_body,new_env =closure_t_rec fundef.body name map_letrec map_type env  in 
    let return,fun_to_add,env_next = closure_t_rec t context closure map_type new_env   in 

    let list_var = Env_closure.get_var_from ~label_fun:name ~env:env_next in
    let updated_env = Env_closure.complete_env ~var_list:list_var ~fun_label:context ~closure:closure ~env:env_next in


    (return, fun_to_add_from_body @[{name=(name);args=List.map (fun (name,_)-> name) fundef.args;body=new_body}]@fun_to_add,updated_env)

and closure_fun_rec(net_let : Net_let.t) (context:string) (closure:Env_closure.closure)(map_type_var:Env_closure.var_type ) (env:Env_closure.env): (all_fun*fundef list*Env_closure.env) =
  match net_let with
  | Net_let.LetRec (fundef,e) -> 
    let (name,type_name) = fundef.name in
    let args_type = fundef.args in
    let map_letrec = Env_closure.add_arg (List.map remove_type args_type) name closure in 
    let new_var_map_with_fun_name = Env_closure.add_type map_type_var name type_name in 
    let new_var_map = Env_closure.add_type_list new_var_map_with_fun_name args_type in
    let new_body,fun_to_add_from_body,env_body =closure_t_rec fundef.body name map_letrec new_var_map env  in 
    let return,fun_to_add,env_next = closure_fun_rec e context closure new_var_map_with_fun_name env_body  in
    
    let list_var = Env_closure.get_var_from ~label_fun:name ~env:env_next in
    let updated_env = Env_closure.complete_env ~var_list:list_var ~fun_label:context ~closure:closure ~env:env_next in




    (return, fun_to_add_from_body @[{name=(name);args=List.map (fun (name,_)-> name) fundef.args;body=new_body}]@fun_to_add,updated_env)
  | other -> 
    let main,fun_to_add,body_env = closure_t_rec other context closure map_type_var env in

    (Main (main),fun_to_add,body_env)

(*--------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
*)

let rec apply_closure_on_let (t:t)(contexte:Id.t)(env:Env_closure.env)(already_added:(string,bool) Hashtbl.t)(fonction_table:fonction_table):t =
  match t with
  
  | IfEq (id,ident,t1,t2)  ->
    let new_t1= apply_closure_t t1 contexte env already_added fonction_table in
    let new_t2= apply_closure_t t2 contexte env already_added fonction_table in
    IfEq(id,ident,new_t1,new_t2)
  |IfLE (id,ident,t1,t2) -> let new_t1 = apply_closure_t t1 contexte env already_added fonction_table in
    let new_t2 = apply_closure_t t2 contexte env already_added fonction_table in
    IfLE(id,ident,new_t1,new_t2)
  | App (id,id_list) -> (
    match Hashtbl.find_opt fonction_table id with
    | None ->  AppClo(id,id_list)
    | Some _ ->
      (match Env_closure.get_var_from ~label_fun:id ~env:env with
    | [] -> App("_"^id,id_list)
    | _ -> AppClo(id,id_list)))
  | _ -> t

and apply_closure_t (t:t)(contexte:Id.t)(env:Env_closure.env)(already_added:(string,bool) Hashtbl.t)(fonction:fonction_table):t=
  match t with
  | Unit -> Unit
  | Int i -> Int i
  | Not id ->  add_let_closure ([id])(Not id)(contexte)(env) (already_added)
              
  | Neg id ->add_let_closure ([id])(Neg id)(contexte)(env)(already_added)
  | Add (id,ident) -> let list = match add_reading_ident ident with 
    | None -> [id]
    | Some id2 -> [id;id2] in 
    add_let_closure list (Add(id,ident))(contexte)(env)(already_added)
  | Sub (id , ident) -> let list = match add_reading_ident ident with 
    | None -> [id]
    | Some id2 -> [id;id2] in
    add_let_closure list (Sub(id,ident))(contexte)(env)(already_added) 
  | Mul (id,ident) -> let list = match add_reading_ident ident with 
    | None -> [id]
    | Some id2 -> [id;id2] in
     add_let_closure list (Mul(id,ident))(contexte)(env)(already_added) 
  | Div (id,ident) -> let list = match add_reading_ident ident with 
    | None -> [id]
    | Some id2 -> [id;id2] in
     add_let_closure list (Div(id,ident))(contexte)(env)(already_added) 
  | IfEq (id,ident,t1,t2) -> let list = match add_reading_ident ident with 
    | None -> [id]
    | Some id2 -> [id;id2] in
    let copy_table = Hashtbl.copy already_added in
     pre_turn_true list contexte env copy_table;
     add_let_closure list (IfEq (id,ident,apply_closure_t t1 contexte env copy_table fonction,apply_closure_t t2 contexte env copy_table fonction))contexte env already_added 

  | IfLE (id,ident,t1,t2) -> let list = match add_reading_ident ident with 
    | None -> [id]
    | Some id2 -> [id;id2] in
    let copy_table = Hashtbl.copy already_added in
     pre_turn_true list contexte env copy_table;
     add_let_closure list (IfLE (id,ident,apply_closure_t t1 contexte env copy_table fonction,apply_closure_t t2 contexte env copy_table fonction))contexte env already_added

    
  | Let ((label), t1 , t2) ->(
                              let list_id_body = get_id_let t1 in 
                              let copy_table = Hashtbl.copy already_added in
                              pre_turn_true list_id_body contexte env copy_table;

                              let list_var = match t1 with
                              | App (id,list_id) -> id::list_id
                              | AppClo (id,list_id) ->id::list_id
                              
                              | _ -> [] in
                              match list_var with
                              | [] -> add_let_closure list_id_body ( Let ((label), apply_closure_on_let (t1) contexte env already_added fonction, apply_closure_t t2 contexte env copy_table fonction))contexte env already_added 
                              | _  -> initialise_closure_smart_list list_var env contexte already_added (
                              Let ((label), apply_closure_on_let (t1) contexte env already_added fonction, apply_closure_t t2 contexte env copy_table fonction) )
  )
  | Var id ->
    (match Env_closure.MapEnv.exists (fun x _not_use -> if x=id then true else false ) env  with
    | true -> initialise_closure id env contexte already_added (Var id)
    | false -> add_let_closure ([id])(Var id)(contexte)(env) already_added )
     
  | App (id,id_list) -> ( 
  
    initialise_closure_smart_list id_list env contexte already_added (
    match Hashtbl.find_opt fonction id with
    | None ->  add_let_closure (id_list) (AppClo(id,id_list)) (contexte) (env) already_added 
    | Some _ ->
      (match Env_closure.get_var_from ~label_fun:id ~env:env with
    | [] -> add_let_closure (id_list) (App("_"^id,id_list)) (contexte) (env) already_added
    | _ -> add_let_closure (id_list) (AppClo(id,id_list)) (contexte) (env) already_added
    ))
      )
  | Tuple (id_list) -> add_let_closure id_list (Tuple(id_list)) contexte env already_added (*TODO after the tuple*)
  | LetTuple (label_list,id,t) -> 
    


    let copy_table = Hashtbl.copy already_added in
    pre_turn_true [id] contexte env copy_table;
    add_let_closure (label_list@[id]) (LetTuple(label_list,id,apply_closure_t t contexte env copy_table fonction)) contexte env already_added 
  | Get (id,ident) -> let list = match add_reading_ident ident with 
    | None -> [id]
    | Some id2 -> [id;id2] in
    add_let_closure list (Get(id,ident))(contexte)(env) already_added 
  | Put (id1,ident,id2) ->let list = match add_reading_ident ident with 
    | None -> [id1]
    | Some id3 -> [id1;id2;id3] in
     add_let_closure list (Put(id1,ident,id2))(contexte)(env) already_added 
  | ExtArray (id) -> add_let_closure ([id]) (ExtArray(id)) contexte env already_added 
  | ExtFunApp (id,id_list) ->add_let_closure (id_list) (ExtFunApp("_"^id,id_list)) contexte env already_added 
  | AppClo (id,id_list) -> 
    initialise_closure_smart_list id_list env contexte already_added (
    ( add_let_closure (id_list) (AppClo(id,id_list)) (contexte) (env) already_added ))
  | New id -> (match id with
  | INT i -> New(INT i)
  | IDENT id -> add_let_closure ([id])(New(IDENT id))(contexte)(env) already_added ) 
let rec apply_closure_fundef (all_fun:all_fun)(env:Env_closure.env)(function_list:fonction_table):all_fun =

  match all_fun with
  | Fun (fun_def,all_fun) -> 
    
    let name = fun_def.name in
    Hashtbl.add function_list name name ;

    let args = fun_def.args in
    let map_var = create_hastbl name env in

    let body = apply_closure_t (fun_def.body) name env map_var function_list in
    let new_fun = {name=(name) ; args=args ; body=body} in
    Fun(new_fun,apply_closure_fundef all_fun env function_list)
  | Main body -> let map_var = create_hastbl "_" env in
    
    Main (apply_closure_t body "_" env map_var function_list) 

let apply_closure (toplevel:toplevel)(env:Env_closure.env):toplevel =
  
  apply_closure_fundef toplevel env (Hashtbl.create 100)

(*---------------------------------------------------------------------------------------------------------------------------------------------------*)
let closure_fun (net_let : Net_let.t) (closure:Env_closure.closure)(map_type:Env_closure.var_type)(env:Env_closure.env):all_fun = 
  let fun_closure,fun_to_add,return_env = closure_fun_rec net_let "_" closure map_type env in
  
  let ret = (add_fun fun_to_add fun_closure) in
  apply_closure ret return_env

(*---------------------------------------------------------------------------------------------------------------------------------------------------*)
let closure (net_let : Net_let.t)  : toplevel = closure_fun net_let Env_closure.MapString.empty  Env_closure.MapString.empty Env_closure.MapEnv.empty
