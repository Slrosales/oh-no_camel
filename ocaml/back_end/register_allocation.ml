(*This file is in charge of outputting ARM assembly*)

let file = "output.s" (*File to write an output*)
let text_section = ".text" (*Section storing the executing code*)
let g_main_section = ".global main" (*Making the function being able to call globally*)
let main_section = "main:" (*main function label*)
let c1 = "mov r0, #0" (*Storing 0 as a return value in register r0*)
let c2 = "bx lr" (*Jumping to the address of the link register and returnning from the function*)

let () =
 let oc = open_out file in
 Printf.fprintf oc "%s\n" text_section;
 Printf.fprintf oc "%s\n" g_main_section;
 Printf.fprintf oc "%s\n" main_section;
 Printf.fprintf oc "\t%s\n" c1;
 Printf.fprintf oc "\t%s\n" c2;
 close_out oc;