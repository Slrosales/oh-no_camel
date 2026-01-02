
(* 
open Asml
 *)

(* let stack_map = Hashtbl.create 50 
let stack_next = ref (-4)

let reset_stack () =  
  Hashtbl.clear stack_map;
  stack_next := -4 

let push_stack id =

  if not (Hashtbl.mem stack_map id) then (

    Hashtbl.add stack_map id !stack_next;
    stack_next := !stack_next - 4 
  )

let find_index id =  
  try Hashtbl.find stack_map id  
  with Not_found ->
    failwith ("Variable " ^ id ^ " not found! Did you define it?")

let rec assembly_maker oc = function
    | Asml.Int(i) ->
      if i > 255 then Printf.fprintf oc "\tldr r0, =%d\n" i
      else Printf.fprintf oc "\tmov r0, #%d\n" i

    | Asml.Var(x) ->
      let index = find_index x in
      Printf.fprintf oc "\tldr r0, [fp, #%d]\n" index

    | Asml.Let((x, _typ), e1, e2) -> 
      assembly_maker oc e1;
      push_stack x;
      let index = find_index x in
      Printf.fprintf oc "\tstr r0, [fp, #%d]\n" index;
      assembly_maker oc e2

    | Asml.Add(x, y) -> 
      let pos_x = find_index x in
      let pos_y = find_index y in

      Printf.fprintf oc "\tldr r0, [fp, #%d]\n" pos_x;
      Printf.fprintf oc "\tldr r1, [fp, #%d]\n" pos_y;
      Printf.fprintf oc "\tadd r0, r0, r1\n"

    | Asml.Sub(x, y) ->
      let pos_x = find_index x in
      let pos_y = find_index y in
      Printf.fprintf oc "\tldr r0, [fp, #%d]\n" pos_x;
      Printf.fprintf oc "\tldr r1, [fp, #%d]\n" pos_y;

      Printf.fprintf oc "\tsub r0, r0, r1\n"
    
    | Asml.Call(id, args) ->
      Printf.fprintf oc "\tbl %s\n" id



 let generate_function oc name body =
  reset_stack ();
  Printf.fprintf oc ".global %s\n" name;
  Printf.fprintf oc "%s:\n" name;
  
  (* PROLOGUE: Save registers and set up the Frame Pointer *)
  Printf.fprintf oc "\tpush {fp, lr}\n";
  Printf.fprintf oc "\tadd fp, sp, #4\n";
  
  (* ALLOCATE STACK: We reserve 128 bytes to be safe for now *)
  Printf.fprintf oc "\tsub sp, sp, #128\n";

  (* MAIN BODY *)
  assembly_maker oc body;

  (* EPILOGUE: Restore stack and return *)
  Printf.fprintf oc "\tsub sp, fp, #4\n";
  Printf.fprintf oc "\tpop {fp, pc}\n" *)




open Syntax

let stack_map = Hashtbl.create 50 
let stack_next = ref (-4)

let reset_stack () = Hashtbl.clear stack_map; stack_next := -4 

let push_stack id =
  if not (Hashtbl.mem stack_map id) then (
    Hashtbl.add stack_map id !stack_next;
    stack_next := !stack_next - 4 
  )

let find_index id =  
  try Hashtbl.find stack_map id  
  with Not_found -> failwith ("Variable not found!")

let rec assembly_maker oc exp = 
  match exp with
    | Int(i) ->
        if i > 255 then Printf.fprintf oc "\tldr r0, =%d\n" i
        else Printf.fprintf oc "\tmov r0, #%d\n" i

    | Var(id) ->
        let index = find_index id in
        Printf.fprintf oc "\tldr r0, [fp, #%d]\n" index

    | Let((id, _typ), e1, e2) -> 
        assembly_maker oc e1;
        push_stack id;
        let index = find_index id in
        Printf.fprintf oc "\tstr r0, [fp, #%d]\n" index;
        assembly_maker oc e2

    | Add(e1, e2) -> 

        assembly_maker oc e1;
        Printf.fprintf oc "\tpush {r0}\n"; 
        assembly_maker oc e2;
        Printf.fprintf oc "\tmov r1, r0\n";
        Printf.fprintf oc "\tpop {r0}\n";
        Printf.fprintf oc "\tadd r0, r0, r1\n"

    | Sub(e1, e2) -> 

        assembly_maker oc e1;
        Printf.fprintf oc "\tpush {r0}\n"; 
        assembly_maker oc e2;
        Printf.fprintf oc "\tmov r1, r0\n";
        Printf.fprintf oc "\tpop {r0}\n";
        Printf.fprintf oc "\tsub r0, r0, r1\n"


    | _ -> failwith "Instruction not implemented yet"

let generate_function oc (id, _typ) body =
  reset_stack ();
  let name = Id.to_string id in
  Printf.fprintf oc ".global %s\n%s:\n" name name;
  Printf.fprintf oc "\tpush {fp, lr}\n\tadd fp, sp, #4\n\tsub sp, sp, #128\n";
  assembly_maker oc body;
  Printf.fprintf oc "\tsub sp, fp, #4\n\tpop {fp, pc}\n"