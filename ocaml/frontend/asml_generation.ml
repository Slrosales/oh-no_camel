module Asml = Core.Asml

let convert_ident (closure : Closure.ident) : Asml.ident_or_imm =
  match closure with INT i -> Asml.INT i | IDENT id -> Asml.IDENT id


let rec mem_store pointer_tuple vars_to_store offset rest =
  match vars_to_store with
  | [] -> rest
  | elem :: elems ->
      Asml.LET
        ( "_",
          Asml.Memstore (pointer_tuple, Asml.INT offset, elem),
          mem_store pointer_tuple elems (offset + 4) rest )

let rec mem_load vars_to_bind pointer_source offset rest =
  match vars_to_bind with
  | [] -> rest
  | _elem :: elems ->
      Asml.LET
        ( "_",
          Asml.MemLoad (pointer_source, Asml.INT offset),
          mem_load elems pointer_source (offset + 4) rest )

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
  | IfEq (left_condition, right_condition, then_body, else_body) ->
      IfEQ
        ( left_condition,
          convert_ident right_condition,
          closure_rec_let then_body,
          closure_rec_let else_body )
  | IfLE (left_condition, right_condition, then_body, else_body) ->
      IfLE
        ( left_condition,
          convert_ident right_condition,
          closure_rec_let then_body,
          closure_rec_let else_body )
  | App (t, t_list) -> Call (t, t_list)
  | Tuple _t_list ->
      failwith "Not supposed to be printed. Tuple for closure_rec"
  | LetTuple (_type_list, _t1, _t2) ->
      failwith "Not supposed to be printed. LetTuple for closure_rec"
  (* Accessing global array label, because 'create_array' is treat as ExtFuncApp in this point *)
  | ExtArray id -> Label id
  | ExtFunApp (name, list_arg) -> Call (name, list_arg)
  | New id -> New (convert_ident id)
  | AppClo (t, t_list) -> Callclo (t, t_list)
  | _ ->
      Printf.eprintf "Uknown command read in asml generation\n";
      exit 1

and closure_rec_let (closure : Closure.t) : Asml.asmt =
  match closure with
  (*Tuple creation*)
  | Let (elem, Tuple args, rest) ->
      let size =
        List.length args * 4
        (*bytes*)
      in
      Asml.LET
        ( elem,
          Asml.New (Asml.INT size),
          mem_store elem args 0 (closure_rec_let rest) )
      (*Tuple as a return value*)
  | Tuple args ->
      let size = List.length args * 4 in
      let temp = Id.genid () in
      Asml.LET
        ( temp,
          Asml.New (Asml.INT size),
          mem_store temp args 0 (EXP (Asml.Ident (Asml.IDENT temp))) )
  (*Tuple destruction*)
  | LetTuple (vars, ptr, rest) -> mem_load vars ptr 0 (closure_rec_let rest)
  | Let ((id), t1, t2) -> LET (id, closure_rec t1, closure_rec_let t2)
  | _ -> EXP (closure_rec closure)

let rec creation_function (closure : Closure.all_fun) : Asml.fundef =
  match closure with
  | Fun (fundef, b) ->
      let tmp_name = fundef.name in
      let name = "_" ^ tmp_name in
      let args = fundef.args in
      let body = closure_rec_let fundef.body in
      let newfundef : Asml.letfun = { name; args; body } in
      Function (newfundef, creation_function b)
  | Main t -> Main (closure_rec_let t)

let asml_generator (closure : Closure.toplevel) : Asml.toplevel =
  creation_function closure
