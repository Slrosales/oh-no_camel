module Id = Core.Id
module Type = Core.Type

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
  | LetRec of fundef * t 
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

val to_string : toplevel -> string
val closure : Net_let.t -> toplevel
