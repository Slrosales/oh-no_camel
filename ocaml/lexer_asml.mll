{
open Parser_asml

(*
open Printf

let print_token = function
  | LET -> print_endline "LET"
  | IN -> print_endline "IN"
  | IF -> print_endline "IF"
  | THEN -> print_endline "THEN"
  | ELSE -> print_endline "ELSE"

  | IDENT s -> printf "IDENT(%s)\n" s
  | LABEL s -> printf "LABEL(%s)\n" s
  | INT i -> printf "INT(%d)\n" i
  | FLOAT f -> printf "FLOAT(%f)\n" f

  | CALL -> print_endline "CALL"
  | CALLCLO -> print_endline "CALLCLO"

  | ADD -> print_endline "ADD"
  | SUB -> print_endline "SUB"
  | MUL -> print_endline "MUL"
  | DIV -> print_endline "DIV"

  | NEG -> print_endline "NEG"
  | FNEG -> print_endline "FNEG"
  | FADD -> print_endline "FADD"
  | FSUB -> print_endline "FSUB"
  | FMUL -> print_endline "FMUL"
  | FDIV -> print_endline "FDIV"

  | MEM -> print_endline "MEM"
  | NEW -> print_endline "NEW"
  | NOP -> print_endline "NOP"
  | NIL -> print_endline "NIL"
  | LE -> print_endline "LE"
  | GE -> print_endline "GE"
  | EQUAL -> print_endline "EQUAL"
  | FEQUAL -> print_endline "FEQUAL"
  | FLE -> print_endline "FLE"
  | ASSIGN -> print_endline "ASSIGN"

  | LPAREN -> print_endline "LPAREN"
  | RPAREN -> print_endline "RPAREN"
  | PLUS -> print_endline "PLUS"
  | UNDERSC -> print_endline "UNDERSC"

  | EOF -> print_endline "EOF"
*)

}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

let ident = "%self" | '?'? lower ( lower | upper | digit | '_' | '.')*
let label = '_' ( lower | upper | digit | '_' | '.')*

rule token = parse
| space+ { token lexbuf }

| "apply_closure"
    { CALLCLO }
| "call_closure"
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
| '.'
    { DOT }
| '?'
    { QUESTION }

|digit+ '.' digit+ as f {let t = FLOAT (float_of_string f) in (*print_token t;*)t }
|digit+ as i { let t = INT (int_of_string i)in (*print_token t;*)t }
|label as l {let t =  LABEL l in (*print_token t;*)t }
| ident as id {let t = IDENT id in (*print_token t;*)t }


| eof
    { (*print_token EOF;*)EOF }
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

