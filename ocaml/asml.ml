
type t =
| Let of Id.t * t * t
| Let of t (**main let _**)
| Add of Id.t * Id.t
| Sub of Id.t * Id.t
| Call of Id.t * Id.t list


type fun = {
  name : Id.t;
  args : Id.t list;
  body : t;
}

type prog = {
  fun_list : fun list;
  main : t;
}

let rec match_t = function
  | Add (x, y) -> add x y
  | Sub (x, y) -> sub x y
  | Let (x, e1, e2) when  ->
  | Let (x, e1,e2) ->
  | Let (_) -> main
  | Call (f, args) ->  
