
module Asml = Core.Asml

let rec closure_rec (closure:Closure.t):Asml.exp = match closure with 
| Unit -> NOP
| Int int -> Ident (INT int)
| Var id -> Label id
| Not _t -> failwith ""(*n'exist peut être avant*)
| Neg t -> Neg t
| Add (t1,t2) -> Add (t1,IDENT t2) (*Can change if improvement*)
| Sub (t1 , t2) -> Sub (t1,IDENT t2)
| Get (t1 , t2) -> failwith "" (*MemLoad (t1,)*)
| Put (t1 , t2 , t3) -> failwith ""


| IfEq (left_condition,right_condition,then_body,else_body) -> failwith ""
| IfLE (left_condition,right_condition,then_body,else_body) -> failwith ""
| App (t , t_list ) -> failwith ""
| Tuple t_list -> failwith ""
| LetTuple ( type_list , t1 , t2 ) -> failwith ""

| ExtArray t -> failwith ""
| ExtFunApp (name,list_arg) -> failwith ""
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