open Printf
type t = 
  | Unit
  | Int of int
  | Float of float 
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t 
  | Var of Id.t
  | Let of (Id.t * Type.t) * t * t

val function : Syntax.t -> t

let rec function exp = 
    match exp with
    | Syntax.unit -> unit
    | Syntax.Int i -> Int i
    | Syntax.Float float_number -> Float float_number
    | Syntax.Var x -> Var x
    | Syntax.Neg e -> insert_let (f e) (fun x -> Neg x)
    | Syntax.Add (e1,e2) -> 
      insert_let (f e1) (fun x1 -> 
      insert_let (f e2) (fun x2) ->
      Add (x1,x2))
    | Syntax.Sub(e1,e2) -> 
      insert_let (f e1) (fun x1 -> 
      insert_let (f e2) (fun x2) ->
      Sub(x1,x2))
    | Syntax.Mul(e1,e2) -> 
      insert_let (f e1) (fun x1 -> 
      insert_let (f e2) (fun x2) ->
      Mul(x1,x2))
    | Syntax.let ((id,ty),e1,e1) -> 
      Let((id,ty),f e1, f e2)