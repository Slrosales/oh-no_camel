
type ident = 
| INT of int
| IDENT of Id.t

type exp =
| NOP
| Ident of ident
| Label of Id.t
| Neg of Id.t
| FNeg of Id.t
| FAdd of Id.t * Id.t
| FSub of Id.t * Id.t
| FMul of Id.t * Id.t
| FDiv of Id.t * Id.t
| New of ident
| Add of Id.t * ident
| Sub of Id.t * ident
| Mul of Id.t * ident
| MemLoad of Id.t * ident
| Memstore of Id.t * ident * Id.t
| IfEQ of Id.t * ident * asmt * asmt 
| IfLE of Id.t * ident * asmt * asmt 
| IfGE of Id.t * ident * asmt * asmt 
| IfFEQ of Id.t * ident * asmt * asmt
| IfFLE of Id.t * ident * asmt * asmt
| Call of Id.t * Id.t list
| Callclo of Id.t * Id.t list
and asmt =
| LET of Id.t * exp * asmt
| EXP of exp

type letfun = {
  name : Id.t;
  args : Id.t list;
  body : asmt;
}
type letfloat = { 
  name : Id.t;
  args : Id.t list;
  body : asmt;
} 

type fundef = 
| Main of asmt
| Letfloat of letfloat * fundef
| Function of letfun * fundef

type toplevel = fundef

let rec print_list_call x =match x with
| [] -> "()"
| elem::[] -> Id.to_string elem
| elem::td -> Id.to_string elem ^ " " ^ print_list_call td 

let ident_to_string (ident:ident):string = match ident with
| INT i -> string_of_int i
| IDENT id -> Id.to_string id

let rec exp_to_string (exp:exp):string = match exp with
| NOP -> "nop"
| Ident ident -> ident_to_string ident
| Label id -> Id.to_string id
| Neg id -> "neg " ^ Id.to_string id
| FNeg id -> "fneg " ^ Id.to_string id
| FAdd (id1,id2) -> "fadd " ^ (Id.to_string id1) ^ " " ^ (Id.to_string id2)
| FSub (id1,id2) -> "fsub " ^ (Id.to_string id1) ^ " " ^ (Id.to_string id2)
| FMul (id1,id2) -> "fMul " ^ (Id.to_string id1) ^ " " ^ (Id.to_string id2)
| FDiv (id1,id2) -> "fDiv " ^ (Id.to_string id1) ^ " " ^ (Id.to_string id2)
| New ident -> "new " ^ ident_to_string ident
| Add (id , ident) -> "add " ^ Id.to_string id ^ " " ^ ident_to_string ident
| Sub (id , ident) -> "sub " ^ Id.to_string id ^ " " ^ ident_to_string ident
| MemLoad (id , int) -> "mem(" ^ Id.to_string id ^ "+" ^ ident_to_string int ^ ")"
| Memstore (id , int1,ident) -> "mem(" ^ Id.to_string id ^ "+" ^ ident_to_string int1 ^ ") <-" ^  Id.to_string ident
| IfEQ (id, ident , asmt1 , asmt2) -> "ifEQ" ^ Id.to_string id ^ " " ^ ident_to_string ident ^ " " ^ asmt_to_string asmt1 ^ " " ^ asmt_to_string asmt2
| IfLE (id, ident , asmt1 , asmt2) -> "ifLE" ^ Id.to_string id ^ " " ^ ident_to_string ident ^ " " ^ asmt_to_string asmt1 ^ " " ^ asmt_to_string asmt2
| IfGE (id, ident , asmt1 , asmt2) -> "ifGE" ^ Id.to_string id ^ " " ^ ident_to_string ident ^ " " ^ asmt_to_string asmt1 ^ " " ^ asmt_to_string asmt2
| IfFLE (id, ident , asmt1 , asmt2) -> "ifFLE" ^ Id.to_string id ^ " " ^ ident_to_string ident ^ " " ^ asmt_to_string asmt1 ^ " " ^ asmt_to_string asmt2
| IfFEQ (id, ident , asmt1 , asmt2) -> "ifFLE" ^ Id.to_string id ^ " " ^ ident_to_string ident ^ " " ^ asmt_to_string asmt1 ^ " " ^ asmt_to_string asmt2
| Call (id , id_list) -> "call " ^ Id.to_string id ^ " " ^ print_list_call id_list
| Callclo (id , id_list) -> "apply_closure " ^ Id.to_string id ^ " " ^ print_list_call id_list
and
asmt_to_string (asmt:asmt):string = match asmt with
| LET (id , exp , asmt) -> "\tlet " ^ Id.to_string id ^ " = " ^ exp_to_string exp ^ " in\n" ^ asmt_to_string asmt
| EXP exp -> "\t" ^ exp_to_string exp
let fundef_string (fundef:fundef):string = match fundef with
| Main asmt -> "let _ =\n" ^ asmt_to_string asmt
| Letfloat (_letfloat , _fundef) -> ""
| Function  (_letfun ,_fundef) -> ""
let to_string (toplevel:toplevel):string = fundef_string toplevel

let write_file ?(output_file="output.asml") (asml:toplevel) :unit =
  let output_file_channel = open_out output_file in
  Printf.fprintf output_file_channel "%s" (to_string asml)