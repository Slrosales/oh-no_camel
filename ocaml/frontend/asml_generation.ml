
module Asml = Core.Asml

let convert_ident (closure:Closure.ident):Asml.ident = match closure with
| INT i -> Asml.INT i
| IDENT id -> Asml.IDENT id

let rec closure_rec (closure:Closure.t):Asml.exp = match closure with 
| Unit -> NOP
| Int int -> Ident (INT int)
| Var id -> Label id
| Not _t -> failwith ""(*n'exist peut être avant*)
| Neg t -> Neg t
| Add (t1,t2) -> Add (t1,convert_ident t2) (*Can change if improvement*)
| Sub (t1 , t2) -> Sub (t1,convert_ident t2)
| Mul (t1 , t2) -> Mul (t1,convert_ident t2)
| Div (t1 , t2) -> Div (t1,convert_ident t2)
| Get (t1 , t2) -> MemLoad (t1,convert_ident t2) 
| Put (t1 , t2 , t3) -> Memstore (t1,convert_ident t2,t3)


| IfEq (_left_condition,_right_condition,_then_body,_else_body) -> failwith ""
| IfLE (_left_condition,_right_condition,_then_body,_else_body) -> failwith ""
| App (_t , _t_list ) -> failwith ""
| Tuple _t_list -> failwith ""
| LetTuple ( _type_list , _t1 , _t2 ) -> failwith ""

| ExtArray _t -> failwith ""
| ExtFunApp (_name,_list_arg) -> failwith "not implemented yet"
| _ -> failwith "Not suppose to be here"
and closure_rec_let (closure:Closure.t):Asml.asmt =match closure with
  | Let ((id , _) , t1 , t2) -> LET (id, closure_rec t1,closure_rec_let t2)
  | _ -> EXP (closure_rec closure)
let rec function_creation (closure:Closure.all_fun):Asml.fundef = match closure with
| Fun (fundef,b) -> 
  let name = fundef.name in
  let args = fundef.args in
  let body = closure_rec_let fundef.body in
  let newfundef:Asml.letfun = {name; args ;body} in
  Function (newfundef,function_creation b)
| Main t -> Main (closure_rec_let t)

let asml_generator (closure:Closure.toplevel):Asml.toplevel = function_creation closure