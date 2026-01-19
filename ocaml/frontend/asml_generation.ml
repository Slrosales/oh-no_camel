module Asml = Core.Asml

let convert_ident (closure : Closure.ident) : Asml.ident_or_imm =
  match closure with INT i -> Asml.INT i | IDENT id -> Asml.IDENT id

let remove_type elem = match elem with
| (name,_) -> name 

let rec closure_rec (closure : Closure.t) : Asml.exp =
  match closure with
  | Unit -> NOP
  | Int int -> Ident (INT int)
  | Var id -> Label id
  | Not _t -> failwith "" (*n'exist peut être avant*)
  | Neg t -> Neg t
  | Add (t1, t2) -> Add (t1, convert_ident t2) (*Can change if improvement*)
  | Sub (t1, t2) -> Sub (t1, convert_ident t2)
  | Get (t1, t2) -> MemLoad (t1, convert_ident t2)
  | Put (t1, t2, t3) -> Memstore (t1, convert_ident t2, t3)
  | IfEq (left_condition, right_condition, then_body, else_body) -> IfEQ (left_condition,convert_ident right_condition,closure_rec_let then_body,closure_rec_let else_body)
  | IfLE (left_condition, right_condition, then_body, else_body) -> IfLE (left_condition,convert_ident right_condition,closure_rec_let then_body,closure_rec_let else_body)
  | App (t, t_list) -> Call(t,t_list)
  | Tuple _t_list -> failwith "Maybe delete in the closure part. There is no Tuple in the asml file"
  | LetTuple (_type_list, _t1, _t2) -> failwith "Maybe delete in the closure part. There is no LetTuple in the asml file"
  | ExtArray _t -> failwith "Maybe delete in the closure part. Note there is no ExtArray in the asml file"
  | ExtFunApp (name, list_arg) -> Call(name,list_arg)
  | New id -> New (convert_ident(id))
  | AppClo (t,t_list) -> Callclo (t,t_list) 
  | _ -> (Printf.eprintf "Uknown command read in asml generation\n";exit(1))

and closure_rec_let (closure:Closure.t):Asml.asmt =match closure with
  | Let ((id,_) , t1 , t2) -> LET (id, closure_rec t1,closure_rec_let t2)
  | _ -> EXP (closure_rec closure)

let rec creation_function (closure : Closure.all_fun) : Asml.fundef =
  match closure with
  | Fun (fundef, b) ->
      let tmp_name,_= fundef.name in
      let name = "_"^tmp_name in
      let args =List.map remove_type fundef.args in
      let body = closure_rec_let fundef.body in
      let newfundef : Asml.letfun = { name; args; body } in
      Function (newfundef, creation_function b)
  | Main t -> Main (closure_rec_let t)

let asml_generator (closure : Closure.toplevel) : Asml.toplevel = creation_function (closure)
