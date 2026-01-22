%{
open Asml
%}

/* tokens */
%token <float> FLOAT
%token <int> INT 
%token <Id.t> LABEL
%token <Id.t> IDENT
%token CALLCLO
%token FNEG
%token FMUL
%token FDIV
%token FSUB
%token FADD
%token THEN
%token ELSE
%token CALL
%token MEM
%token LET
%token NEG
%token ADD
%token SUB
%token MUL
%token DIV
%token NEW
%token NOP
%token IF
%token IN
%token FLE
%token FEQUAL
%token ASSIGN
%token LE
%token GE
%token PLUS
%token EQUAL
%token UNDERSC
%token LPAREN
%token RPAREN
%token EOF
%token NIL
%token DOT          
%token QUESTION    
%token EOF
%token CALL
%token LET EQUAL IN
%token MEM PLUS ASSIGN LPAREN RPAREN
%token EOF

%type <Asml.toplevel> toplevel
%start toplevel

%%

ident_or_imm:
| INT   { INT ($1) }
| IDENT { IDENT ($1)}

label_or_ident:
| LABEL { $1 }
| IDENT { $1 }

exp:
| NOP { NOP }
| INT { Ident (INT ($1)) }
| IDENT { Ident (IDENT ($1)) }
| LABEL { Label ($1) }
| NEG IDENT { Neg ($2) }
| ADD IDENT ident_or_imm { Add ($2,$3) }
| SUB IDENT ident_or_imm { Sub ($2,$3) }
| MUL IDENT ident_or_imm { Mul ($2,$3) }
| DIV IDENT ident_or_imm { Div ($2,$3) }
| IF IDENT EQUAL ident_or_imm THEN asmt ELSE asmt
    { IfEQ ($2, $4, $6, $8) }
| IF IDENT LE ident_or_imm THEN asmt ELSE asmt
    { IfLE ($2, $4, $6, $8) }
| IF IDENT GE ident_or_imm THEN asmt ELSE asmt
    { IfGE ($2, $4, $6, $8) }
| MEM LPAREN IDENT PLUS ident_or_imm RPAREN
    { MemLoad ($3,$5) }
| MEM LPAREN IDENT PLUS ident_or_imm RPAREN ASSIGN IDENT
    { Memstore ($3,$5,$8) }

| CALL label_or_ident formal_args
    { Call ($2, $3) }
| CALLCLO IDENT formal_args
    { Callclo ($2, $3) }

| NEW ident_or_imm { New ($2) }

asmt:
| LET IDENT EQUAL exp IN asmt
    { LET ($2, $4, $6) }

| LET UNDERSC EQUAL exp IN asmt
    { LET ("_", $4, $6) } 

| LPAREN asmt RPAREN { $2 }
| exp { EXP ($1) }

formal_args:
| IDENT formal_args { $1 :: $2 }
| IDENT { [$1] }
|  { [] } 

fundefs:
| LET UNDERSC EQUAL asmt
    { Main ($4) }
| LET LABEL formal_args EQUAL asmt fundefs
    { Function ({ name = $2 ; args = $3 ; body = $5 }, $6) }

toplevel:
| fundefs EOF { $1 }