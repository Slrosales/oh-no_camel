type Id = 
| INT
| IDENT

type t =
| Float of float
| Int of int
| Bool of bool
| Neg of Id.t
| FNeg of Id.t
| If of Id.t * Id.t * Id.t * Id.t * Id.t
| Let of Id.t * t * t
| Let of t (**main let _**)
| Var of Id.t
| Add of Id.t * Id.t
| Sub of Id.t * Id.t
| FAdd of Id.t * Id.t
| FSub of Id.t * Id.t
| FMul of Id.t * Id.t
| FDiv of Id.t * Id.t
| Assign of Id.t * Id.t
| Equal of Id.t * Id.t
| LE of Id.t * Id.t
| GE of Id.t * Id.t
| FEqual of Id.t * Id.t
| FLE of Id.t * Id.t
| Mem of Id.t * int
| Call of Id.t * Id.t list
| New of Id.t 


 
type fun = {
  name : Id.t;
  args : Id.t list;
  body : t;
}

type prog = {- 
  fun_list : fun list;
  main : t;
}
