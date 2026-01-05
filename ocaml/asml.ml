
type ident = 
| INT of int
| IDENT of Id.t

type condition =
| EQ
| LE
| GE
| FEQ
| FLE

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
| MemLoad of Id.t * int
| Memstore of Id.t * int * Id.t
| If of Id.t * condition * ident * asmt * asmt 
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

let condition_to_string (condition:condition):string = match condition with
| EQ -> "EQ"
| LE -> "LE"
| GE -> "GE"
| FEQ -> "FEQ"
| FLE -> "FLE"

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
| MemLoad (id , int) -> "mem(" ^ Id.to_string id ^ "+" ^ string_of_int int ^ ")"
| Memstore (id , int1,ident) -> "mem(" ^ Id.to_string id ^ "+" ^ string_of_int int1 ^ ") <-" ^  Id.to_string ident
| If (id , condition , ident , asmt1 , asmt2) -> "if" ^ condition_to_string condition ^ " " ^ Id.to_string id ^ " " ^ ident_to_string ident ^ " " ^ asmt_to_string asmt1 ^ " " ^ asmt_to_string asmt2
| Call (id , id_list) -> "call " ^ Id.to_string id ^ " " ^ print_list_call id_list
| Callclo (id , id_list) -> "apply_closure " ^ Id.to_string id ^ " " ^ print_list_call id_list
and
asmt_to_string (asmt:asmt):string = match asmt with
| LET (id , exp , asmt) -> "let " ^ Id.to_string id ^ " = " ^ exp_to_string exp ^ " in " ^ asmt_to_string asmt
| EXP exp -> exp_to_string exp
let rec fundef_string (fundef:fundef):string = match fundef with
| Main asmt -> "let _ = " ^ asmt_to_string asmt
| Letfloat (letfloat , fundef) -> failwith ""
| Function  (letfun ,fundef) -> failwith ""
let to_string (toplevel:toplevel):string = fundef_string toplevel


(*to finish*)
let rec closure_rec (syntax_t:Syntax.t):asmt = match syntax_t with 
| Unit -> failwith ""
| Bool bool -> failwith "create error here"
| Int int -> failwith ""
| Float float -> failwith ""
| Not t -> failwith ""
| Neg t -> failwith ""
| Add (t1,t2) -> failwith ""
| Sub (t1 , t2) -> failwith ""
| FNeg t -> failwith ""
| FAdd (t1 , t2) -> failwith ""
| FSub (t1 , t2) -> failwith ""
| FMul (t1 , t2) -> failwith ""
| FDiv (t1 , t2) -> failwith ""
| Eq (t1 , t2) -> failwith ""
| LE (t1 , t2) -> failwith ""
| If (cond , t1 , t2) -> failwith ""
| Let ((id , _) , t1 , t2) -> failwith ""
| Var id -> failwith ""
| LetRec (fundef , t) -> failwith ""
| App (t , t_list ) -> failwith ""
| Tuple t_list -> failwith ""
| LetTuple ( type_list , t1 , t2 ) -> failwith ""
| Array (elem , elem_list) -> failwith ""
| Get (t1 , t2) -> failwith ""
| Put (t1 , t2 , t3) -> failwith ""

let rec function_creation (syntax_t:Syntax.t):fundef = match syntax_t with
| LetRec (fundef,b) -> failwith ""
| _ -> Main (closure_rec syntax_t)

let closure (syntax_t:Syntax.t):toplevel = function_creation syntax_t