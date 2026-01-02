open Emit

(* let print_ast l =
  print_string (Syntax.to_string (Parser.exp Lexer.token l)); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    print_ast (Lexing.from_channel inchan);

    close_out oc;
    (* Emit.output_ARM (); *)
  with e -> (close_in inchan; raise e)

let () = 
  let files = ref [] in
  Arg.parse
    [ ]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files *)


let file f = 
  let inchan = open_in f in
  print_endline ("Reading file: " ^ f);
  try
    let lexbuf = Lexing.from_channel inchan in
    let ast = Parser.exp Lexer.token lexbuf in 
    let oc = open_out "output.s" in
    

    Emit.generate_function oc (Id.genid (), Type.Int) ast;
    close_out oc;
    close_in inchan;
    print_endline "Success! Check output.s"
  with e -> (close_in inchan; raise e)


let () = 
  if Array.length Sys.argv > 1 then
    file Sys.argv.(1)
  else
    print_endline "Usage: ./mincamlc <filename>"