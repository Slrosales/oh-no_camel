{
open Parser_asml


}



let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

let ident = lower ( lower | upper | digit | '_')*
let label = '_' (ident | ".%self" )

rule token = parse
| space+ { token lexbuf }

(*key word*)
| "apply_closure"
    { CALLCLO }
| "fneg"
    { FNEG }
| "fmul"
    { FMUL }
| "fdiv"
    { FDIV }
| "fsub"
    { FSUB }
| "fadd"
    { FADD }
| "then"
    { THEN }
| "else"
    { ELSE }
| "call"
    { CALL }
| "mem"
    { MEM }
| "let"
    { LET }
| "neg"
    { NEG }
| "add"
    { ADD }
| "sub"
    { SUB }
| "mul"
    { MUL }
| "div"
    { DIV }
| "new"
    { NEW }
| "nop"
    { NOP }
| "if"
    { IF }
| "in"
    { IN }
(* operation*)
| "<=."
    { FLE }
| "=."
    { FEQUAL }
| "<-"
    { ASSIGN }
| "<="
    { LE }
| ">="
    { GE }
(*other symbole*)
| "(*"
    { comment lexbuf; 
      token lexbuf }
| '(' 
    { LPAREN }
| ')'
    { RPAREN }
| '+'
    { PLUS }
| '='
    { EQUAL }
| '_'
    { UNDERSC }
(*direct value*)
(*Int and float*)
|digit+ '.' digit+ as f { FLOAT (float_of_string f) }
|digit+ as i {  INT (int_of_string i) }
|label as l {  LABEL l }
| ident as id { IDENT id }
| eof
    { EOF }
| _
    { failwith
	(Printf.sprintf "unknown token %s near characters %d-%d"
	   (Lexing.lexeme lexbuf)
	   (Lexing.lexeme_start lexbuf)
	   (Lexing.lexeme_end lexbuf)) }

and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }

