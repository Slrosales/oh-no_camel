module Id = Core.Id
module Type =Core.Type
module MapString = Map.Make(String)
module MapEnv = Map.Make(String)
module MapVar= Map.Make(String)


type var_type = Type.t MapString.t
type closure =  Id.t MapString.t
type var = int MapVar.t 
type env = var MapEnv.t

(*
--------------------------------------------------------------------
Print to a map
--------------------------------------------------------------------
*)
(*------------------------------ Type map --------------------------------*)

let rec to_string_var_rec (var_type:(string*Type.t) list) =
  match var_type with
  | [] -> ""
  | (var,type_var)::[] -> var ^" -> "^ Type.to_string type_var ^ "]"
  | (var,type_var )::tail -> var^" -> "^ Type.to_string type_var^","^to_string_var_rec tail

let to_string_var (var_type:var_type):string =
  "var_type =["^to_string_var_rec (MapString.to_list var_type)

let print_var (var_type:var_type):unit= print_endline (to_string_var var_type)

(*------------------------------ closure ---------------------------------*)
let rec string_closure_rec (closure: (string*string) list): string =
  match closure with
  | [] -> "]"
  | (key,name)::[] -> key^"->"^name ^"]"
  | (key,name)::tail ->  key^"->"^name^","^ string_closure_rec tail

let string_closure (closure: (string*string) list) =
  match closure with
  | [] -> "[]"
  | elem -> "["^string_closure_rec elem 

let print_closure (closure:closure)(context:string):unit =
  print_endline ("map : "^(string_closure (MapString.to_list closure))^" context : "^context )

(*------------------------------ var ---------------------------------*)
let rec string_var_list_rec (var:(string*int)list):string =
  match var with
  | [] -> "]"
  | (key,(int))::[] -> "\t"^key^" -> "^string_of_int int^"]"
  | (key,(int))::tail -> "\t"^key^" -> "^string_of_int int^"\n"^string_var_list_rec tail
let string_var_list (var:(string*int)list):string =
  match var with
  | [] -> "[]"
  | (key,(int))::[] -> "[\n\t"^key^" -> "^string_of_int int^"]"
  | (key,(int))::tail -> "[\n\t"^key^" -> "^string_of_int int^",\n"^string_var_list_rec tail

let string_var(var:var):string =
  string_var_list (MapVar.to_list var)

(*------------------------------ env ---------------------------------*)
let rec string_env_list_rec (env:(string*var)list):string = 
  match env with
  | [] -> "]"
  | (key,varmap)::[] -> key^"-> "^string_var varmap^"]"
  |  (key,varmap)::tail ->key^"-> "^string_var varmap^"\n"^string_env_list_rec tail
let string_env_list (env:(string*var)list):string = match env with
| [] -> "[]"
| l -> "["^string_env_list_rec l


let string_env (env:(env)) :string =
  "env : "^(string_env_list (MapEnv.to_list env))
  
  
let print_env (env:env) : unit = 
  print_endline (string_env env)


(*
--------------------------------------------------------------------
function
--------------------------------------------------------------------
*)
(*------------------------------ Type map --------------------------------*)

let add_type (var_type: var_type) (id: Id.t)  (type_id:Type.t) : var_type=
  MapString.add id type_id var_type

let rec add_type_list (var_type:var_type) (id_list :(Id.t*Type.t) list) : var_type =
  match id_list with
  | [] -> var_type
  |(e,type_e)::tail -> let ret = add_type var_type e type_e in add_type_list ret tail
let get_type  (var_type:var_type)  (id:Id.t) : Type.t option = MapString.find_opt id var_type 
(*------------------------------ closure ---------------------------------*)
let rec add_arg_rec (args:Id.t list)(funct_name:string)(map:closure) =
  match args with
  | [] -> map
  | e:: tail ->let new_map = MapString.add e funct_name map in
    add_arg_rec tail funct_name new_map

let add_arg (args:Id.t list)(funct_name:string)(map:closure)=
  add_arg_rec args funct_name map

let add_element (ident : Id.t) (funct_name:string )(map: closure)=
  MapString.add ident funct_name map 

let rec add_element_list (ident_list: Id.t list) (funct_name:string)(map:closure)=
  match ident_list with
  | [] -> map
  | e::list ->  add_element_list list funct_name (add_element e funct_name map)

let get_data (ident:Id.t)(map : closure):string option=
  MapString.find_opt ident map

let need_closure_bool (ident:Id.t) (context:string)(map : closure) :bool =
   match get_data ident map with 
   | None -> false
   | Some s when s!=context ->  true
   | Some _ -> false

   
let rec list_need_closure (ids:string list)(context:string)(map_type:var_type)(map : closure):((string )list) =
  match ids with
  | [] -> []
  | e::tail ->

    match need_closure_bool e context map with
    | true -> (e)::list_need_closure tail context map_type map 
    | false -> list_need_closure tail context map_type map


(*------------------------------ var ---------------------------------*)
let var_in (ident:Id.t)(varmap:var  ):bool=
  match MapVar.find_opt ident varmap with
  | None -> false
  | Some _ -> true

let get_key (var:var):string list =
  let list_var = MapVar.to_list var in
  List.map (fun (key,_) -> key) list_var

let get_var ~(id:Id.t) ~(varmap:var): int option =
  match MapVar.find_opt id varmap with
  | None -> None
  | Some s -> Some s

(*------------------------------ env ---------------------------------*)
let update_env (label_fun:Id.t)(new_var:var)(env:env):env =
  let new_env = MapEnv.remove label_fun env in
  MapEnv.add label_fun new_var new_env

let add_var_env ~(label_fun:Id.t) ~(ident:Id.t) ~(env : env):env =
  
  match MapEnv.find_opt label_fun env with
  | None -> let new_var_map = MapVar.empty in 
            let ident_var = ident in
            let var_map = MapVar.add ident_var (4) new_var_map in
            MapEnv.add label_fun var_map env
  | Some s -> (let ident_var = ident in
    match var_in ident_var s with
  | true -> env
  | false -> let number_of_var = (MapVar.cardinal s + 1 ) *4 in
  let ident_var = ident in
    let new_var_map = MapVar.add ident_var (number_of_var) s in
    update_env label_fun new_var_map env )

let rec add_var_env_from_list ~(label_fun:Id.t) ~(id_list:(Id.t) list) ~(env:env):env =
  match id_list with
  | [] -> env
  | e::tail -> add_var_env_from_list ~label_fun:label_fun ~id_list:tail ~env:(add_var_env ~label_fun:label_fun ~ident:e ~env:env)

let get_var_from ~(label_fun:Id.t) ~(env:env) : string list = 
   match MapEnv.find_opt label_fun env with
  | None -> []
  | Some s -> get_key s

let get_var_in_env ~(label_fun:Id.t) ~(id:Id.t) ~(env:env) : (int) option =

  match MapEnv.find_opt label_fun env with
  | None -> None
  | Some s -> get_var ~id:id ~varmap:s

let rec complete_env ~(var_list:(string) list) ~(fun_label:string) ~(closure:closure) ~(env:env) :env =
  match var_list with
  | [] -> env
  | (name)::tail ->
              let funct_name = get_data name closure in 
              match funct_name with
              | None -> complete_env ~var_list:tail ~fun_label:fun_label ~closure:closure ~env:env
              | Some f when f=fun_label -> complete_env ~var_list:tail ~fun_label:fun_label ~closure:closure ~env:env
              | Some _ -> let new_env = add_var_env ~label_fun:fun_label ~ident:(name) ~env:env in 
                          complete_env ~var_list:tail ~fun_label:fun_label ~closure:closure ~env:new_env