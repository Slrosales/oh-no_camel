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
| EXP of exp

type letfun = {
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

type toplevel = fundef