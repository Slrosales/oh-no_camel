(* type ident = 
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
| Float of float
| Int of int
| Bool of bool
| Var of IDENT
| Neg of IDENT
| FNeg of IDENT
| FAdd of IDENT * IDENT
| FSub of IDENT * IDENT
| FMul of IDENT * IDENT
| FDiv of IDENT * IDENT
| New of ident
| Add of IDENT * ident
| Sub of IDENT * ident
| MemLoad of IDENT * int
| Memstore of IDENT * int * IDENT
| If of IDENT * condition * ident * asmt * asmt 
| Call of IDENT * IDENT list
| Callclo of IDENT * IDENT list
and asmt =
| LET of IDENT * exp * asmt
| EXP of exp *)



(* type letfun = {
  name : IDENT;
  args : IDENT list;
  body : asmt;
}
type letfloat = { 
  name : IDENT;
  args : IDENT list;
  body : asmt;
} 

type fundef = 
| Main of asmt
| Letfloat of letfloat * fundef
| Function of letfun * fundef

type toplevel = fundef *)


(* OCaml types MUST be lowercase *)
type ident_t = Id.t 

type ident = 
  | INT of int
  | IDENT of ident_t

type condition = EQ | LE | GE | FEQ | FLE

type exp =
  | NOP
  | Float of float
  | Int of int
  | Bool of bool
  | Var of ident_t
  | Neg of ident_t
  | FNeg of ident_t
  | FAdd of ident_t * ident_t
  | FSub of ident_t * ident_t
  | FMul of ident_t * ident_t
  | FDiv of ident_t * ident_t
  | New of ident
  | Add of ident_t * ident
  | Sub of ident_t * ident
  | MemLoad of ident_t * int
  | Memstore of ident_t * int * ident_t
  | If of ident_t * condition * ident * asmt * asmt 
  | Call of ident_t * ident_t list
  | Callclo of ident_t * ident_t list
and asmt =
  | LET of ident_t * exp * asmt
  | EXP of exp

type letfun = {
  name : ident_t;
  args : ident_t list;
  body : asmt;
}
type letfloat = { 
  name : ident_t;
  args : ident_t list;
  body : asmt;
} 

type fundef = 
  | Main of asmt
  | Letfloat of letfloat * fundef
  | Function of letfun * fundef

type toplevel = fundef