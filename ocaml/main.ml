<<<<<<< HEAD
open Emit

=======
module Syntax = Core.Syntax
module Parser = Core.Parser
module Lexer = Core.Lexer
open Frontend
(*option*)

let output_file = ref None


let _usage_message = ""
let info_set = "TODO write info\n"
let version = "v0.1"
let verbose = ref false
let type_check_only = ref false
let parse_only = ref false
let output_asml = ref false
let output_asml_file=ref None
let asml_only = ref false
(*code*)
>>>>>>> 935b25cbf70d15a28b0bc0bb90bacfcb23e33491
let print_ast l =
  print_string (Syntax.to_string l); print_newline ()

let file f = 
  let inchan = open_in f in
  try
<<<<<<< HEAD
    print_ast (Lexing.from_channel inchan);
    Emit.output_ARM ();
=======
    let file_script = (Parser.exp Lexer.token (Lexing.from_channel  inchan)) in
    print_ast file_script;


    (*k_normalization*)
    let test_output_k = K_normalization.normalization_function file_script in
     print_ast test_output_k;
    (*alpha_generation*)
    
    (*reduction of nested-let*)
    
    (*closure conversion*)

    (*ASML generation*)

    (*other transformations*)

    (*Take care of input asml here*)

    (*Register allocation*)

    (*ARM allocation*)
    

>>>>>>> 935b25cbf70d15a28b0bc0bb90bacfcb23e33491
    close_in inchan


  with e -> (close_in inchan; raise e)

let () = 
  let files = ref [] in
  Arg.parse
    [("-o",Arg.String (fun (s) -> output_file := Some s ),"output file name");
      ("-h",Arg.Unit (fun () -> print_string info_set;exit(0)),"helper");
      ("-help",Arg.Unit (fun ()-> print_string info_set;exit(0)),"helper");
      ("-v",Arg.Unit (fun () -> print_string version;exit(0)),"Print version");
      ("-verbose",Arg.Set verbose,"Activate verbose");
      ("-t",Arg.Set type_check_only,"Only execute up to type checking");
      ("-p",Arg.Set parse_only ,"Only execute up to parsing");
      ("-asml",Arg.Set output_asml,"Produce asml_output_file");
      ("-asml_output",Arg.String (fun (s) -> output_asml_file := Some s ),"output asml file name");
      ("-asml_only",Arg.Set asml_only,"Only execute upt to asml")]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s [-h/-help] [-v] [-verbose] [-t] [-p] [-asml] [-asml_only] [-asml_output output_asml_file] [-o output_file] filenames" Sys.argv.(0));
  
  
  
    List.iter
    (fun f -> ignore (file f)) 
    !files

