open Core

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

type fundef = { name : Id.t; args : Id.t list; body : t }
(*rename label by adding _*)

type all_fun = Fun of fundef * all_fun | Main of t
type toplevel = all_fun

val to_string : toplevel -> string
val closure : Net_let.t -> toplevel
