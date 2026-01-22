module Asml = Core.Asml

type operande = Add | Sub | Mul | Div

let add_assotiation (string:string)(int:int)(var_assoctiation:(string,int)Hashtbl.t):unit =
  match Hashtbl.find_opt var_assoctiation string with
  | None -> Hashtbl.add var_assoctiation string int
  | Some _ -> ()

let init_var_dependency (string:string)(var_depedency:(string,int)Hashtbl.t):unit =
   match Hashtbl.find_opt var_depedency string with
  | None -> Hashtbl.add var_depedency string 0
  | Some _ ->()

let add_var_dependency (string:string)(var_depedency:(string,int)Hashtbl.t):unit =
  match Hashtbl.find_opt var_depedency string with
  | None -> Hashtbl.add var_depedency string 1
  | Some int ->Hashtbl.remove var_depedency string; Hashtbl.add var_depedency string (int+1) 

let rec init_list(str_list:string list)(var_depedency:(string,int)Hashtbl.t):unit=
  match str_list with
  | [] -> ()
  | e::tail -> add_var_dependency e var_depedency;init_list tail var_depedency

let init_optimization_ident (asml:Asml.ident_or_imm)(var_depedency:(string,int)Hashtbl.t):unit =
  match asml with
  | INT _ -> ()
  | IDENT id -> add_var_dependency id var_depedency 

let rec init_optimization_rec (asml:Asml.exp)(var_assoctiation:(string,int)Hashtbl.t)(var_depedency:(string,int)Hashtbl.t):unit=
  match asml with
  | Float _ 
  | Label _
  | NOP -> ()
  | New ident | Ident ident -> init_optimization_ident ident var_depedency
  | FNeg id | Neg id -> add_var_dependency id var_depedency
  | FAdd (id1,id2) | FSub (id1,id2) | FMul(id1,id2)|FDiv(id1,id2) -> add_var_dependency id1 var_depedency ;add_var_dependency id2 var_depedency
  | Add (id,ident) | Sub(id,ident) | Div(id,ident) | Mul(id,ident) -> add_var_dependency id var_depedency;init_optimization_ident ident var_depedency
  | IfLE(id,ident,asmt1,asmt2) | IfGE(id,ident,asmt1,asmt2)|IfEQ(id,ident,asmt1,asmt2) -> add_var_dependency id var_depedency ;
init_optimization_ident ident var_depedency;fonction_optimization_init asmt1 var_assoctiation var_depedency;fonction_optimization_init asmt2 var_assoctiation var_depedency;
  | IfFEQ(id1,id2,asmt1,asmt2) |IfFLE(id1,id2,asmt1,asmt2) ->add_var_dependency id1 var_depedency ;add_var_dependency id2 var_depedency ;fonction_optimization_init asmt1 var_assoctiation var_depedency;fonction_optimization_init asmt2 var_assoctiation var_depedency;
  |MemLoad (id,ident) -> add_var_dependency id var_depedency;init_optimization_ident ident var_depedency
  |Memstore (id1,ident1,id2) -> add_var_dependency id1 var_depedency;init_optimization_ident ident1 var_depedency;add_var_dependency id2 var_depedency
  | Call(_,id_list) -> init_list id_list var_depedency
  |Callclo(id,id_list) -> add_var_dependency id var_depedency;init_list id_list var_depedency

and fonction_optimization_init (asmt:Asml.asmt)(var_assoctiation:(string,int)Hashtbl.t)(var_depedency:(string,int)Hashtbl.t)=
  match asmt with
  | LET(label,Ident (INT i),asmt) -> add_assotiation label i var_assoctiation;init_var_dependency label var_depedency ; fonction_optimization_init asmt var_assoctiation var_depedency
  | LET(_,exp,asmt) -> init_optimization_rec exp var_assoctiation var_depedency;fonction_optimization_init asmt var_assoctiation var_depedency
  | EXP exp -> init_optimization_rec exp var_assoctiation var_depedency 

let produce_operation(op:operande)(op1:string)(op2:Asml.ident_or_imm):Asml.exp=
  match op with
  | Add -> Add(op1,op2)
  | Sub -> Sub(op1,op2)
  | Mul -> Mul(op1,op2)
  | Div -> Div(op1,op2)

let apply_operation(op:operande)(op1:int)(op2:int):Asml.exp=
  match op with
  | Add -> Ident(INT (op1+op2))
  | Sub -> Ident(INT (op1-op2))
  | Mul -> Ident(INT (op1*op2))
  | Div -> Ident(INT (op1/op2))
let operation_with_switch (op:operande)(id:string)(ident:Asml.ident_or_imm)(opt:int)(var_assoctiation:(string,int)Hashtbl.t):(Asml.exp*bool)=
  let op1 = Hashtbl.find_opt var_assoctiation id in
                      match ident with
                      | INT i -> (match op1 with
                                  | None -> (produce_operation op id ident,false)
                                  | Some op1_i ->(apply_operation op op1_i i),true)
                      | IDENT id_op2 -> (let op2 = Hashtbl.find_opt var_assoctiation id_op2 in
                                        match (op1,op2) with
                                        | (None,None) -> (produce_operation op id ident,false)
                                        | (None,Some i2) -> (produce_operation op id (INT i2),false)
                                        | (Some i1,None) -> if opt >= 2 then (
                                                            (produce_operation op id_op2 (INT i1) ),true)
                                                            else (produce_operation op id ident,false)
                                        | (Some i1,Some i2) ->(apply_operation op i1 i2,true) )

let operation_without_switch (op:operande)(id:string)(ident:Asml.ident_or_imm)(var_assoctiation:(string,int)Hashtbl.t):(Asml.exp*bool)=
  let op1 = Hashtbl.find_opt var_assoctiation id in
                      match ident with
                      | INT i -> (match op1 with
                                  | None -> (produce_operation op id ident,false)
                                  | Some op1_i ->(apply_operation op op1_i i),true)
                      | IDENT id_op2 -> (let op2 = Hashtbl.find_opt var_assoctiation id_op2 in
                                        match (op1,op2) with
                                        | (None,None) -> (produce_operation op id ident,false)
                                        | (None,Some i2) -> (produce_operation op id (INT i2),false)
                                        | (Some i1,Some i2) -> (apply_operation op i1 i2,false)
                                        | _ -> (produce_operation op id ident,true))

let rec fonction_optimization_recurrency_exe(exe:Asml.exp)(opt:int) (var_assoctiation:(string,int)Hashtbl.t)(var_depedency:(string,int)Hashtbl.t):(Asml.exp*bool) =
  match exe with
  | Float f -> (Float f,false)
  | Label id ->( match Hashtbl.find_opt var_assoctiation id with (*NEED TO BE REMOVE WHEN LABEL AND VAR ARE IDENTIFY*)
                | None -> (Label id,false) 
                | Some i -> (Ident (INT i)),true)
  | NOP -> (NOP,false) 
  | New ident -> (match ident with
    | IDENT id -> (match Hashtbl.find_opt var_assoctiation id with
                        | None -> (New(IDENT id),false)
                        | Some i -> (New(INT i)),true)
    |INT i -> (New(INT i ),false)) 
                          
  | Ident (ident) -> (
    match ident with
    | IDENT id -> (match Hashtbl.find_opt var_assoctiation id with
                        | None ->(Ident(IDENT id),false)
                        | Some i -> (Ident(INT i),true))
    | INT i -> (Ident(INT i ),false)) 
  | FNeg id ->  (FNeg id,false)
  | Neg id -> (Neg id,false)
  | FAdd (id1,id2) -> (FAdd(id1,id2),false)
  | FSub (id1,id2) -> (FSub (id1,id2),false)
  | FMul(id1,id2) -> (FMul (id1,id2),false)
  | FDiv(id1,id2) -> (FDiv(id1,id2),false)
  | Add (id,ident) -> operation_with_switch Add id ident opt var_assoctiation 
  | Sub(id,ident) -> operation_without_switch Sub id ident var_assoctiation 
  | Div(id,ident) -> operation_without_switch Div id ident var_assoctiation
  | Mul(id,ident) -> operation_with_switch Mul id ident opt var_assoctiation 
  | IfLE (id1,ident,asmt1,asmt2) ->(match ident with
  | IDENT id2 -> (match Hashtbl.find_opt var_assoctiation id2 with
                                        | None ->
                                          let asmt1_ret,bool_asmt1 = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
                                          let asmt2_ret,bool_asmt2 = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in
                                          (IfLE (id1,IDENT(id2),asmt1_ret,asmt2_ret),bool_asmt1||bool_asmt2)
                                        | Some i ->
                                          let asmt1_ret,_ = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
                                          let asmt2_ret,_ = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in 
                                          (IfLE (id1,INT(i),asmt1_ret,asmt2_ret),true) )
                                    | INT _ -> 
                                      let asmt1_ret,bool_asmt1 = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
                                          let asmt2_ret,bool_asmt2 = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in
                                          (IfLE (id1,ident,asmt1_ret,asmt2_ret),bool_asmt1||bool_asmt2))
  | IfGE (id1,ident,asmt1,asmt2) ->(match ident with
  | IDENT id2 -> (match Hashtbl.find_opt var_assoctiation id2 with
                                        | None ->
                                          let asmt1_ret,bool_asmt1 = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
                                          let asmt2_ret,bool_asmt2 = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in
                                          (IfGE (id1,IDENT(id2),asmt1_ret,asmt2_ret),bool_asmt1||bool_asmt2)
                                        | Some i ->
                                          let asmt1_ret,_ = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
                                          let asmt2_ret,_ = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in 
                                          (IfGE (id1,INT(i),asmt1_ret,asmt2_ret),true) )
                                    | INT _ -> 
                                      let asmt1_ret,bool_asmt1 = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
                                          let asmt2_ret,bool_asmt2 = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in
                                          (IfGE (id1,ident,asmt1_ret,asmt2_ret),bool_asmt1||bool_asmt2))
| IfEQ (id1,ident,asmt1,asmt2) ->(match ident with
  | IDENT id2 -> (match Hashtbl.find_opt var_assoctiation id2 with
                                        | None ->
                                          let asmt1_ret,bool_asmt1 = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
                                          let asmt2_ret,bool_asmt2 = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in
                                          (IfEQ (id1,IDENT(id2),asmt1_ret,asmt2_ret),bool_asmt1||bool_asmt2)
                                        | Some i ->
                                          let asmt1_ret,_ = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
                                          let asmt2_ret,_ = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in 
                                          (IfEQ (id1,INT(i),asmt1_ret,asmt2_ret),true) )
                                    | INT _ -> 
                                      let asmt1_ret,bool_asmt1 = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
                                          let asmt2_ret,bool_asmt2 = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in
                                          (IfEQ (id1,ident,asmt1_ret,asmt2_ret),bool_asmt1||bool_asmt2))
  | IfFEQ(id1,id2,asmt1,asmt2) -> 
    let asmt1_ret,bool_asmt1 = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
    let asmt2_ret,bool_asmt2 = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in
    (IfFEQ(id1,id2,asmt1_ret,asmt2_ret),bool_asmt1||bool_asmt2)
  |IfFLE(id1,id2,asmt1,asmt2) ->
    let asmt1_ret,bool_asmt1 = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
    let asmt2_ret,bool_asmt2 = fonction_optimization_recurrency_asmt asmt2 opt var_assoctiation var_depedency in
    (IfFLE(id1,id2,asmt1_ret,asmt2_ret),bool_asmt1||bool_asmt2)
  |MemLoad (id,ident) -> (MemLoad(id,ident),false)
  |Memstore (id1,ident1,id2) -> (Memstore(id1,ident1,id2),false)
  | Call(id,id_list) -> (Call(id,id_list),false)
  |Callclo(id,id_list) -> (Callclo(id,id_list),false) 
and fonction_optimization_recurrency_asmt(asmt:Asml.asmt)(opt:int)(var_assoctiation:(string,int)Hashtbl.t)(var_depedency:(string,int)Hashtbl.t):(Asml.asmt*bool) =
  match asmt with
  | LET (id,exp,asmt1) ->
    let exp_ret,bool_exe = fonction_optimization_recurrency_exe exp opt var_assoctiation var_depedency in
    let asmt_ret,bool_asmt = fonction_optimization_recurrency_asmt asmt1 opt var_assoctiation var_depedency in
    (LET (id,exp_ret,asmt_ret),bool_exe||bool_asmt)
  | EXP (exp) -> 
    let exp_ret,bool_exe = fonction_optimization_recurrency_exe exp opt var_assoctiation var_depedency in
    (EXP(exp_ret),bool_exe)

let rec delete_useless_asmt (asmt:Asml.asmt)(var_assoctiation:(string,int)Hashtbl.t)(var_depedency:(string,int)Hashtbl.t):Asml.asmt=
  match asmt with
  | LET (id,exp,asmt) -> (match Hashtbl.find_opt var_assoctiation id with
    | Some i -> (match Hashtbl.find_opt var_depedency id with 
                            | Some i when i=0 -> delete_useless_asmt asmt var_assoctiation var_depedency
                            | Some _  | None -> LET (id,exp,delete_useless_asmt asmt var_assoctiation var_depedency))
   | None -> LET(id,exp,delete_useless_asmt asmt var_assoctiation var_depedency))
  
  | EXP (exp) -> EXP(exp) 

let delete_useless (asmt:Asml.asmt)(var_assoctiation:(string,int)Hashtbl.t)(var_depedency:(string,int)Hashtbl.t):Asml.asmt=
  delete_useless_asmt asmt var_assoctiation var_depedency

let rec optimisation_recurrency_launcher (asmt:Asml.asmt)(opt:int)(var_assoctiation:(string,int)Hashtbl.t)(var_depedency:(string,int)Hashtbl.t):Asml.asmt=

  let new_asmt,bool_ret =fonction_optimization_recurrency_asmt asmt opt var_assoctiation var_depedency in
  let table_assotiation = Hashtbl.create 100 in
  let table_depedency = Hashtbl.create 100 in 
  fonction_optimization_init new_asmt table_assotiation table_depedency;

  if bool_ret then( optimisation_recurrency_launcher new_asmt opt table_assotiation table_depedency) else (delete_useless new_asmt var_assoctiation table_depedency)

let fonction_optimization_lunch (asmt:Asml.asmt)(opt:int):Asml.asmt=
  let table_assotiation:(string,int)Hashtbl.t = Hashtbl.create 100  in
  let table_depedency:(string,int)Hashtbl.t = Hashtbl.create 100 in
  fonction_optimization_init asmt table_assotiation table_depedency;
  let new_asmt =optimisation_recurrency_launcher asmt opt table_assotiation table_depedency in

  new_asmt


let rec optimisation_local_rec (asml:Asml.fundef)(opt:int):Asml.fundef =match asml with
| Function (let_fun,fundef) ->
  let _asmt_ret = fonction_optimization_lunch (let_fun.body) opt in
  Function(let_fun,optimisation_local_rec fundef opt)
| Main (body) -> Main(fonction_optimization_lunch (body) opt)
| Letfloat (float,funded) -> Letfloat(float,optimisation_local_rec funded opt)

let optimisation_local (asml:Asml.toplevel)(opt:int):Asml.toplevel=optimisation_local_rec asml opt