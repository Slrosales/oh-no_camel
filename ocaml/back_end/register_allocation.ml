(*This file is in charge of outputting ARM assembly*)

let output_file = "output.s" (*File to write an output*)
let text_section = ".text" (*Section storing the executing code*)
let g_main_section = ".global main" (*Making the function being able to call globally*)
let main_section = "main:" (*main function label*)

let output_ARM() =

 let ic = open_in

 let oc = open_out output_file in
 Printf.fprintf oc "%s\n" text_section;
 Printf.fprintf oc "%s\n" g_main_section;
 Printf.fprintf oc "%s\n" main_section;
 close_out oc;