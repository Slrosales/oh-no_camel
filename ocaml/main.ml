module Syntax = Core.Syntax
module Parser = Core.Parser
module Lexer = Core.Lexer
module Asml = Core.Asml
module Type = Core.Type
module Lexer_asml = Core.Lexer_asml
module Parser_asml = Core.Parser_asml
open Frontend
open Back_end

let output_file = ref None
let _usage_message = ""
let info_set = "TODO write info\n"
let version = "v0.1"
let verbose = ref false
let type_check_only = ref false
let parse_only = ref false
let output_asml = ref false
let output_asml_file = ref None
let asml_only = ref false
let opt = ref 0
let asml_input_mode = ref false

let print_ast l =
  print_string (Syntax.to_string l);
  print_newline ()

let print_knorm k =
  print_string (K_normalization.to_string k);
  print_newline ()

let file f =
  let inchan = open_in f in

  (* Change done here by backend  *)
  let arm_name = match !output_file with Some s -> s | None -> "output.s" in
  let oc = open_out arm_name in

  try
    let asml = 
    match !asml_input_mode with
    | false -> (
    (* 1. Parsing *)
    let file_script = Parser.exp Lexer.token (Lexing.from_channel inchan) in
    if !verbose then (
      print_endline "--- AST ---";
      print_ast file_script);

    if !parse_only then exit 0;

    (* 1.5 Type Checking *)
    let typed_script =
      try Typechecking.f file_script
      with Typechecking.Type_Error (e, t_expected, t_actual) ->
        Printf.eprintf "\n[Type Error]\n";
        Printf.eprintf "  Expected: %s\n" (Type.to_string t_expected);
        Printf.eprintf "  Actual:   %s\n" (Type.to_string t_actual);
        exit 1
    in

    if !type_check_only then exit 0;
    Printf.printf "Ending: Type Checking\n\n";

    (* 2. K-Normalization *)
    Printf.printf "Starting: K-Norm\n";
    let output_k = K_normalization.f typed_script in
    if !verbose then (
      print_endline "\n--- K-Norm ---";
      print_knorm output_k);

    (* 3. Alpha Conversion *)
    Printf.printf "Starting: Alpha conversion\n";
    let output_alpha = Alpha_conversion.alpha output_k in
      if !verbose then (print_endline "--- Alpha ---"; print_knorm output_alpha);
    Printf.printf "Ending: Alpha conversion\n\n";

    (*reduction of nested-let*)
    Printf.printf "Starting: Nested-let\n";
    let nested_expression = Net_let.net_let output_alpha in
    if !verbose then (print_endline "--- Let-Net ---"; print_endline (Net_let.to_string nested_expression));
    Printf.printf "Ending: Nested-let\n\n";

    (*closure conversion*)
    Printf.printf "Starting: Closure\n";
    let closure_expression = Closure.closure nested_expression in
    if !verbose then (print_endline "--- Closure ---"; print_endline (Closure.to_string closure_expression));
    Printf.printf "Ending: Closure\n\n";

    (*ASML generation*)
    Printf.printf "Starting: ASML\n";
    Asml_generation.asml_generator closure_expression)
    | true -> (Parser_asml.toplevel Lexer_asml.token (Lexing.from_channel inchan)) in
    if !parse_only then (print_endline "\n--- ASML ---";
                        print_endline (Asml.to_string asml);
                        exit(0);
      );
    if !verbose then (
      print_endline "\n--- ASML ---";
      print_endline (Asml.to_string asml));
    if !output_asml then (
      if !verbose then print_endline "\n--- WRITING ASML FILE ---";
      match !output_asml_file with
      | None -> Asml.write_file asml
      | Some s -> Asml.write_file ~output_file:s asml
      );
    Printf.printf "Ending: ASML\n\n";

    Printf.printf "Start optimization: \n\n";
    let new_asml = if !opt != 0 then (Optimisation_local.optimisation_local asml !opt) else asml in
    Printf.printf "End_optimization \n\n";
    if !verbose then (
      print_endline "\n--- ASML ---";
      print_endline (Asml.to_string new_asml));
    Register_allocation.generate_assembly oc new_asml;

    (*ARM allocation*)
    close_out oc;
    close_in inchan
  with e ->
    close_out oc;
    close_in inchan;
    raise e

let () =
  let files = ref [] in
  Arg.parse
    [
      ("-o", Arg.String (fun s -> output_file := Some s), "output file name");
      ( "-h",
        Arg.Unit
          (fun () ->
            print_string info_set;
            exit 0),
        "helper" );
      ( "-help",
        Arg.Unit
          (fun () ->
            print_string info_set;
            exit 0),
        "helper" );
      ( "-v",
        Arg.Unit
          (fun () ->
            print_string version;
            exit 0),
        "Print version" );
      ("-verbose", Arg.Set verbose, "Activate verbose");
      ("-type_only", Arg.Set type_check_only, "Only execute up to type checking");
      ("-parsing_only", Arg.Set parse_only, "Only execute up to parsing");
      ("-asml_input_mode",Arg.Set asml_input_mode,"Read asml file from the input file you enter");
      ("-asml", Arg.Set output_asml, "Produce asml_output_file");
      ( "-asml_output",
        Arg.String (fun s -> output_asml_file := Some s),
        "output asml file name" );
      ("-O",Arg.Int (fun i -> opt:= i),"Choose a level of Optimisation:\n\t0 = None (default),\n\t1 = remove any let var = integer that are useless and do add,sub,mul,div that have both integer as operand\n\t2 same as 1 but can swap var and imidiate value in add or mul if need it" )
    ]
    (fun s -> files := !files @ [ s ])
    (Printf.sprintf
       "usage: %s [-h/-help] [-v] [-verbose] [-type_only] [-parsing_only] [-asml_input_mode] [-asml] \
        [-asml_output output_asml_file] [-o output_file] filenames"
       Sys.argv.(0));

  List.iter (fun f -> ignore (file f)) !files
