module Syntax = Core.Syntax
module Parser = Core.Parser
module Lexer = Core.Lexer
module Asml = Core.Asml
open Frontend

let output_file = ref None


let _usage_message = ""
let info_set = "TODO write info\n"
let version = "v0.1"
let verbose = ref false
let type_check_only = ref false
let parse_only = ref false
let output_asml = ref false
let output_asml_file= ref None
let asml_only = ref false
(*code*)
let print_ast l =
  print_string (Syntax.to_string l); print_newline ()

let print_knorm k =
  print_string (K_normalization.to_string k); print_newline ()

let file f = 
  let inchan = open_in f in

  (* Change done here by backend  *)
  let arm_name = match !output_file with Some s -> s | None -> "output.s" in
  let oc = open_out arm_name in

  try
    (* 1. Parsing *)
    let file_script = (Parser.exp Lexer.token (Lexing.from_channel  inchan)) in
    if !verbose then (print_endline "--- AST ---"; print_ast file_script);

    (* 2. K-Normalization *)
    let output_k = K_normalization.f file_script in
        if !verbose then (print_endline "--- K-Norm ---"; print_knorm output_k); 

    (* 3. Alpha Conversion *)
    let output_alpha = Alpha_conversion.alpha output_k in
      if !verbose then (print_endline "--- Alpha ---"; print_knorm output_alpha);
      
    (*reduction of nested-let*)
    
    let nested_expression = Net_let.net_let output_alpha in
    if !verbose then (print_endline "--- Let-Net ---"; print_endline (Net_let.to_string nested_expression));

    (*closure conversion*)

    let closure_expression = Closure.closure nested_expression in
    if !verbose then (print_endline "--- Closure ---"; print_endline (Closure.to_string closure_expression));

    (*ASML generation*)

    let asml = Asml_generation.asml_generator closure_expression in
    if !verbose then (print_endline "--- ASML ---"; print_endline (Asml.to_string asml));
    if !output_asml then 
      (if !verbose then (print_endline "--- WRITING ASML FILE ---");
      match !output_asml_file with
      | None -> Asml.write_file asml
      | Some s -> Asml.write_file ~output_file:s asml
      );
    (*other transformations*)

    (*Take care of input asml here*)


    Register_allocation.generate_assembly oc asml;


    (*ARM allocation*)
      (* Change done here by backend  *)
    close_out oc;
    close_in inchan


  with e -> (close_out oc; close_in inchan; raise e)

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

