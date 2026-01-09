module Syntax = Core.Syntax
module Parser = Core.Parser
module Lexer = Core.Lexer
module Asml = Core.Asml
module Type = Core.Type
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
    (* 1. Parsing *)
    let file_script = Parser.exp Lexer.token (Lexing.from_channel inchan) in
    if !verbose then (
      print_endline "--- AST ---";
      print_ast file_script);

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
    let asml = Asml_generation.asml_generator closure_expression in
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

    Register_allocation.generate_assembly oc asml;

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
      ("-t", Arg.Set type_check_only, "Only execute up to type checking");
      ("-p", Arg.Set parse_only, "Only execute up to parsing");
      ("-asml", Arg.Set output_asml, "Produce asml_output_file");
      ( "-asml_output",
        Arg.String (fun s -> output_asml_file := Some s),
        "output asml file name" );
      ("-asml_only", Arg.Set asml_only, "Only execute upt to asml");
    ]
    (fun s -> files := !files @ [ s ])
    (Printf.sprintf
       "usage: %s [-h/-help] [-v] [-verbose] [-t] [-p] [-asml] [-asml_only] \
        [-asml_output output_asml_file] [-o output_file] filenames"
       Sys.argv.(0));

  List.iter (fun f -> ignore (file f)) !files
