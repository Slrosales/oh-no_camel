(* module Syntax = Core.Syntax
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
let output_asml_file=ref None
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
    let test_output_k = K_normalization.f file_script in
        if !verbose then print_endline "--- K-Norm ---"; print_knorm test_output_k; 

    (* 3. Alpha Conversion *)
    let test_output_alpha = Alpha_conversion.alpha test_output_k in
      if !verbose then (print_endline "--- Alpha ---"; print_knorm test_output_alpha);
      
    (*reduction of nested-let*)
    
    let nested_expression = Net_let.net_let test_output_alpha in
    if !verbose then print_endline "--- Let-Net ---"; print_endline (Net_let.to_string nested_expression);

    (*closure conversion*)

    let closure_expression = Closure.closure nested_expression in
    if !verbose then print_endline "--- Closure ---"; print_endline (Closure.to_string closure_expression);

    (*ASML generation*)

    let asml = Asml_generation.asml_generator closure_expression in
    if !verbose then print_endline "--- ASML ---"; print_endline (Asml.to_string asml);

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
    !files *)


(* module Syntax = Core.Syntax
module Parser = Core.Parser
module Lexer = Core.Lexer
module Asml = Core.Asml
open Frontend

let output_file = ref None
let info_set = "MinCaml to ARM Compiler\nUsage: ./mincamlc [options] files\n"
let version = "v0.1"
let verbose = ref false
let type_check_only = ref false
let parse_only = ref false
let output_asml = ref false
let output_asml_file = ref None
let asml_only = ref false
let asml_input_mode = ref false


let register_alloc_mode = ref None

let print_ast l =
  print_string (Syntax.to_string l); print_newline ()

let print_knorm k =
  print_string (K_normalization.to_string k); print_newline ()

let file f = 
  let inchan = open_in f in
  let arm_name = match !output_file with Some s -> s | None -> "output.s" in
  let oc = open_out arm_name in

  try
    if !asml_input_mode then begin
      if !verbose then print_endline "Reading direct ASML input...";
      let lexbuf = Lexing.from_channel inchan in
      let asml_prog = Parser_asml.toplevel Lexer_asml.token lexbuf in
      Part_2.generate_assembly oc asml_prog
    end 
    else begin
      let file_script = Parser.exp Lexer.token (Lexing.from_channel inchan) in
      if !verbose then (print_endline "--- AST ---"; print_ast file_script);

      let knorm = K_normalization.f file_script in
      if !verbose then (print_endline "--- K-Norm ---"; print_knorm knorm);

      let alpha = Alpha_conversion.alpha knorm in
      let nested = Net_let.net_let alpha in
      let closure = Closure.closure nested in
      let asml = Asml_generation.asml_generator closure in

      if !verbose then print_endline "--- Generating Assembly ---";
      Part_2.generate_assembly oc asml
    end;
    close_out oc;
    close_in inchan
  with e -> 
    close_out oc; 
    close_in inchan; 
    raise e

let () = 
  let files = ref [] in
  Arg.parse
    [("-o", Arg.String (fun s -> output_file := Some s), "output file name");
     ("-asml_input", Arg.Set asml_input_mode, "Run the ASML directly");
     ("-h", Arg.Unit (fun () -> print_string info_set; exit 0), "helper");
     ("-help", Arg.Unit (fun () -> print_string info_set; exit 0), "helper");
     ("-v", Arg.Unit (fun () -> print_string version; exit 0), "Print version");
     ("-verbose", Arg.Set verbose, "Activate verbose");
     ("-t", Arg.Set type_check_only, "Only execute up to type checking");
     ("-p", Arg.Set parse_only, "Only execute up to parsing");
     ("-asml", Arg.Set output_asml, "Produce asml_output_file");
     ("-asml_output", Arg.String (fun s -> output_asml_file := Some s), "output asml file name");
     ("-asml_only", Arg.Set asml_only, "Only execute up to asml");
     ("-reg_alloc", Arg.String (fun s -> register_alloc_mode := Some s), "output asml file name")]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s [options] filenames" Sys.argv.(0));
  
  List.iter (fun f -> ignore (file f)) !files *)





module Syntax = Core.Syntax
module Parser = Core.Parser
module Lexer = Core.Lexer
module Asml = Core.Asml
open Frontend


let output_file = ref None
let info_set = "MinCaml to ARM Compiler\nUsage: ./mincamlc [options] files\n"
let version = "v0.1"
let verbose = ref false
let type_check_only = ref false
let parse_only = ref false
let output_asml = ref false
let output_asml_file = ref None
let asml_only = ref false
let asml_input_mode = ref false


let register_alloc_mode = ref "lc"

let print_ast l =
  print_string (Syntax.to_string l); print_newline ()

let print_knorm k =
  print_string (K_normalization.to_string k); print_newline ()


let file f = 
  let inchan = open_in f in
  let arm_name = match !output_file with Some s -> s | None -> "output.s" in
  let oc = open_out arm_name in

  try

    let asml_prog = 
      if !asml_input_mode then begin
        if !verbose then print_endline "Reading direct ASML input...";
        let lexbuf = Lexing.from_channel inchan in
        Parser_asml.toplevel Lexer_asml.token lexbuf
      end 
      else begin
        let file_script = Parser.exp Lexer.token (Lexing.from_channel inchan) in
        if !parse_only then (print_ast file_script; exit 0);
        
        let knorm = K_normalization.f file_script in
        let alpha = Alpha_conversion.alpha knorm in
        let nested = Net_let.net_let alpha in
        let closure = Closure.closure nested in
        let asml = Asml_generation.asml_generator closure in
        
        if !asml_only then (

          (match !output_asml_file with
           | Some name -> let as_oc = open_out name in output_string as_oc "ASML generated"; close_out as_oc
           | None -> ());
          exit 0
        );
        asml
      end 
    in


    match !register_alloc_mode with
    | "spill" -> 
        if !verbose then print_endline "--- Using Spill Backend (register_allocation.ml) ---";
        Register_allocation.reset_stack ();
        Register_allocation.generate_assembly oc asml_prog

    | "lc" | _ -> 
        if !verbose then print_endline "--- Using LC Backend ---";
        let rec allocate_all p = match p with
          | Asml.Function (f, next) -> 
              Part_1.allocate_registers f.body f.args; 
              allocate_all next
          | Asml.Main asmt -> 
              Part_1.allocate_registers asmt []
          | _ -> () 
        in
        allocate_all asml_prog;
        
        Part_2.generate_assembly oc asml_prog;

    close_out oc;
    close_in inchan
  with e -> 
    close_out oc; 
    close_in inchan; 
    raise e


let () = 
  let files = ref [] in
  Arg.parse
    [("-o", Arg.String (fun s -> output_file := Some s), "Output ARM assembly file name");
     ("-asml_input", Arg.Set asml_input_mode, "Read .asml file directly instead of .ml");
     ("-reg_alloc", Arg.Set_string register_alloc_mode, "Register allocation mode: 'lc' (Linear Scan) or 'spill' (Full Spill)");
     ("-v", Arg.Unit (fun () -> print_endline version; exit 0), "Print version");
     ("-verbose", Arg.Set verbose, "Show debug traces");
     ("-t", Arg.Set type_check_only, "Stop after type checking");
     ("-p", Arg.Set parse_only, "Stop after parsing");
     ("-asml", Arg.Set output_asml, "Enable ASML output");
     ("-asml_output", Arg.String (fun s -> output_asml_file := Some s), "Specify ASML output file");
     ("-asml_only", Arg.Set asml_only, "Stop after generating ASML");
     ("-h", Arg.Unit (fun () -> print_string info_set; exit 0), "Show help");
     ("-help", Arg.Unit (fun () -> print_string info_set; exit 0), "Show help")]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "Usage: %s [options] filenames" Sys.argv.(0));
  
  List.iter (fun f -> ignore (file f)) !files