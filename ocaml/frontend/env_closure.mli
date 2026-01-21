module Id = Core.Id
module Type = Core.Type
module MapString : Map.S with type key = string
module MapEnv : Map.S with type key = string
module MapVar : Map.S with type key = string 

type var_type = Type.t MapString.t
type closure =  Id.t MapString.t
type var = int MapVar.t
type env = var MapEnv.t

(*
--------------------------------------------------------------------
Printint to a map
--------------------------------------------------------------------

*)
val to_string_var : var_type -> string 

val print_var : var_type  -> unit
(*------------------------------ closure ---------------------------------*)
val string_closure : (string * string) list -> string
val print_closure : closure -> string -> unit
(*------------------------------ var ---------------------------------*)
val string_var_list :(string*int)list -> string
val string_var :var -> string
(*------------------------------ env ---------------------------------*)
val string_env_list :(string*var)list -> string
val string_env :(env) -> string
val print_env :env -> unit
(*
--------------------------------------------------------------------
function
--------------------------------------------------------------------
*)
(*------------------------------ Type map --------------------------------*)

val add_type : var_type -> Id.t -> Type.t -> var_type

val add_type_list : var_type -> (Id.t*Type.t) list -> var_type

val get_type : var_type -> Id.t -> Type.t option

(*------------------------------ closure ---------------------------------*)

val add_arg :
  Id.t list -> string -> closure -> closure

val add_element :
  Id.t -> string -> closure -> closure

val add_element_list : Id.t list -> string -> closure -> closure

val get_data :
  Id.t -> closure -> string option

val need_closure_bool : Id.t -> string ->  closure -> bool

val list_need_closure : string list -> string -> var_type -> closure->string list

(*------------------------------ var ---------------------------------*)
val var_in : Id.t -> var-> bool

val get_key : var -> string list

val get_var :
  id:Id.t -> varmap:var -> int  option

(*------------------------------ env ---------------------------------*)

val update_env : Id.t ->  var-> env  -> env

val add_var_env :
  label_fun:Id.t ->
  ident:Id.t ->
  env:env ->
  env


val add_var_env_from_list :
  label_fun:Id.t ->
  id_list:Id.t  list ->
  env:env ->
  env

val get_var_from : label_fun:Id.t -> env:env -> string list

val get_var_in_env :
  label_fun:Id.t ->
  id:Id.t ->
  env:env ->
  int option

val complete_env :
  var_list:string  list ->
  fun_label:string ->
  closure:closure ->
  env:env ->
  env
