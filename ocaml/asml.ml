
type ident_or_imm = 
| INT of int
| IDENT of Id.t

type exp =
| NOP
| Ident of ident_or_imm
| Float of float 
| Label of Id.t
| Neg of Id.t
| FNeg of Id.t
| FAdd of Id.t * Id.t
| FSub of Id.t * Id.t
| FMul of Id.t * Id.t
| FDiv of Id.t * Id.t
| New of ident_or_imm
| Add of Id.t * ident_or_imm
| Sub of Id.t * ident_or_imm
| Mul of Id.t * ident_or_imm
| Div of Id.t * ident_or_imm
| MemLoad of Id.t * ident_or_imm
| Memstore of Id.t * ident_or_imm * Id.t
| IfEQ of Id.t * ident_or_imm * asmt * asmt 
| IfLE of Id.t * ident_or_imm * asmt * asmt 
| IfGE of Id.t * ident_or_imm * asmt * asmt 
| IfFEQ of Id.t * Id.t * asmt * asmt
| IfFLE of Id.t * Id.t * asmt * asmt
| Call of Id.t * Id.t list
| Callclo of Id.t * Id.t list
and asmt =
| LET of Id.t * exp * asmt
| EXP of exp

type letfun = { name : Id.t; args : Id.t list; body : asmt }
type letfloat = { name : Id.t; float_let : float }

type fundef =
  | Main of asmt
  | Letfloat of letfloat * fundef
  | Function of letfun * fundef

type toplevel = fundef

let rec print_list_call x =
  match x with
  | [] -> "()"
  | elem :: [] -> Id.to_string elem
  | elem :: td -> Id.to_string elem ^ " " ^ print_list_call td

let rec print_list_args x =
  match x with
  | [] -> ""
  | elem :: [] -> Id.to_string elem
  | elem :: td -> Id.to_string elem ^ " " ^ print_list_args td


let ident_to_string (ident_or_imm : ident_or_imm) : string =
  match ident_or_imm with INT i -> string_of_int i | IDENT id -> Id.to_string id

let rec exp_to_string (exp:exp):string = match exp with
| NOP -> "nop"
| Ident ident_or_imm -> ident_to_string ident_or_imm
| Label id -> Id.to_string id
| Float f -> string_of_float f
| Neg id -> "neg " ^ Id.to_string id
| FNeg id -> "fneg " ^ Id.to_string id
| FAdd (id1,id2) -> "fadd " ^ (Id.to_string id1) ^ " " ^ (Id.to_string id2)
| FSub (id1,id2) -> "fsub " ^ (Id.to_string id1) ^ " " ^ (Id.to_string id2)
| FMul (id1,id2) -> "fMul " ^ (Id.to_string id1) ^ " " ^ (Id.to_string id2)
| FDiv (id1,id2) -> "fDiv " ^ (Id.to_string id1) ^ " " ^ (Id.to_string id2)
| New ident_or_imm -> "new " ^ ident_to_string ident_or_imm
| Add (id , ident_or_imm) -> "add " ^ Id.to_string id ^ " " ^ ident_to_string ident_or_imm
| Sub (id , ident_or_imm) -> "sub " ^ Id.to_string id ^ " " ^ ident_to_string ident_or_imm
| Mul (id, ident_or_imm) -> "mul " ^ Id.to_string id ^ " " ^ ident_to_string ident_or_imm
| Div (id, ident_or_imm) -> "div " ^ Id.to_string id ^ " " ^ ident_to_string ident_or_imm
| MemLoad (id , int) -> "mem(" ^ Id.to_string id ^ "+" ^ ident_to_string int ^ ")"
| Memstore (id , int1,ident_or_imm) -> "mem(" ^ Id.to_string id ^ "+" ^ ident_to_string int1 ^ ") <-" ^  Id.to_string ident_or_imm
| IfEQ (id, ident_or_imm , asmt1 , asmt2) -> "ifEQ " ^ Id.to_string id ^ "=" ^ ident_to_string ident_or_imm ^ " then (\n" ^ asmt_to_string asmt1 ^ "\n)\nelse (\n" ^ asmt_to_string asmt2 ^ "\n)"
| IfLE (id, ident_or_imm , asmt1 , asmt2) -> "ifLE " ^ Id.to_string id ^ "<=" ^ ident_to_string ident_or_imm ^ " then (\n" ^ asmt_to_string asmt1 ^ "\n)\nelse (\n" ^ asmt_to_string asmt2 ^ "\n)"
| IfGE (id, ident_or_imm , asmt1 , asmt2) -> "ifGE " ^ Id.to_string id ^ ">=" ^ ident_to_string ident_or_imm ^ " then (\n" ^ asmt_to_string asmt1 ^ " \n)\nelse (\n" ^ asmt_to_string asmt2 ^ "\n)"
| IfFLE (id, id2 , asmt1 , asmt2) -> "ifFLE " ^ Id.to_string id ^ "<=" ^ id2 ^ " then (\n" ^ asmt_to_string asmt1 ^ "\n)\nelse (\n" ^ asmt_to_string asmt2 ^ "\n)"
| IfFEQ (id, id2 , asmt1 , asmt2) -> "ifFEQ " ^ Id.to_string id ^ "=" ^ id2 ^ " then (\n" ^ asmt_to_string asmt1 ^ "\n)\nelse (\n" ^ asmt_to_string asmt2 ^ "\n)"
| Call (id , id_list) -> "call (" ^ Id.to_string id ^ "," ^ print_list_call id_list ^")"
| Callclo (id , id_list) -> "apply_closure (" ^ Id.to_string id ^ "," ^ print_list_call id_list^")"
and
asmt_to_string (asmt:asmt):string = match asmt with
| LET (id , exp , asmt) -> "\tlet " ^ Id.to_string id ^ " = " ^ exp_to_string exp ^ " in\n" ^ asmt_to_string asmt
| EXP exp -> "\t" ^ exp_to_string exp
let rec fundef_string (fundef:fundef):string = match fundef with
| Main asmt -> "let _ =\n" ^ asmt_to_string asmt
| Letfloat (letfloat , fundef) -> "let " ^ letfloat.name ^ " = " ^ (string_of_float letfloat.float_let) ^ "\n" ^ (fundef_string fundef)
| Function  (letfun ,fundef) -> "let " ^letfun.name ^" "^(print_list_args letfun.args)^" =(\n"^asmt_to_string letfun.body ^"\n)\n" ^ fundef_string fundef
let to_string (toplevel:toplevel):string = fundef_string toplevel


let write_file ?(output_file = "output.asml") (asml : toplevel) : unit =
  let output_file_channel = open_out output_file in
  Printf.fprintf output_file_channel "%s" (to_string asml)
