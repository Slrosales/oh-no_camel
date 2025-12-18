(*This file is in charge of outputting ARM assembly*)

let input_file = "/home/js/Documents/Compilation_Project/oh-no_camel/asml/print_zero.asml" (*Just an example for now, change it to the right path on your PC*)
let output_file = "output.s" (*File to write an output*)
let text_section = ".text" (*Section storing the executing code*)
let g_main_section = ".global main" (*Making the function being able to call globally*)
let main_section = "main:" (*main function label*)

let read_files() =

 let ic = open_in input_file in 
 try
  let rec read_all() =
  try
    let line = input_line ic in
    print_endline line;
    read_all()
  with End_of_file -> ()
  in

  read_all();
  flush stdout;
  close_in ic
with e ->
  close_in_noerr ic;
  raise e

let output_ARM() =

 let oc = open_out output_file in
 Printf.fprintf oc "%s\n" text_section;
 Printf.fprintf oc "%s\n" g_main_section;
 Printf.fprintf oc "%s\n" main_section;
 close_out oc

 let () =
  read_files();
  output_ARM();