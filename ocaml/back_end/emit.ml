
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




open Asml

let stack_map = Hashtbl.create 50 
let stack_next = ref (-4)

let reset_stack () = Hashtbl.clear stack_map; stack_next := -4 

let push_stack (IDENT id) =
  if not (Hashtbl.mem stack_map id) then (
    Hashtbl.add stack_map id !stack_next;
    stack_next := !stack_next - 4 
  )

let find_index (IDENT id) =  
  try 
    Hashtbl.find stack_map id  
  with 
  | Not_found -> 
      Printf.printf "Variable not found: %s\n" (id.to_string id);
      failwith ("Variable not found: " ^ (Id.to_string id))

(* 1. Helper to load 'ident' types (could be INT or IDENT) into a register *)
let load_ident oc id reg =
  match id with
  | INT i -> 
      if i > 255 then Printf.fprintf oc "\tldr %s, =%d\n" reg i
      else Printf.fprintf oc "\tmov %s, #%d\n" reg i
  | IDENT _ as id_obj ->
      let index = find_index id_obj in
      Printf.fprintf oc "\tldr %s, [fp, #%d]\n" reg index

(* 2. Helper to evaluate basic expressions - Result always goes to r0 *)
let rec emit_exp oc = function
  | Int i -> 
      if i > 255 then Printf.fprintf oc "\tldr r0, =%d\n" i
      else Printf.fprintf oc "\tmov r0, #%d\n" i
  | Var id -> 
      load_ident oc (IDENT id) "r0"
  | Neg id ->
      load_ident oc (IDENT id) "r0";
      Printf.fprintf oc "\trsb r0, r0, #0\n"
  | Add (id1, id2) ->
      load_ident oc (IDENT id1) "r0";
      load_ident oc id2 "r1";
      Printf.fprintf oc "\tadd r0, r0, r1\n"
  | Sub (id1, id2) ->
      load_ident oc (IDENT id1) "r0";
      load_ident oc id2 "r1";
      Printf.fprintf oc "\tsub r0, r0, r1\n"
  | FMul (id1, id2) ->
      load_ident oc (IDENT id1) "r0";
      load_ident oc (IDENT id2) "r1";
      Printf.fprintf oc "\tmul r0, r0, r1\n"
  | If (id1, cond, id2, then_asmt, else_asmt) ->
      let label_id = Id.to_string (Id.genid ()) in
      let else_lbl = ".L_else_" ^ label_id in
      let exit_lbl = ".L_exit_" ^ label_id in
      load_ident oc (IDENT id1) "r0";
      load_ident oc id2 "r1";
      Printf.fprintf oc "\tcmp r0, r1\n";
      let branch = match cond with 
        | EQ -> "bne" | LE -> "bgt" | GE -> "blt" | _ -> "bne" in
      Printf.fprintf oc "\t%s %s\n" branch else_lbl;
      emit_asmt oc then_asmt;
      Printf.fprintf oc "\tb %s\n%s:\n" exit_lbl else_lbl;
      emit_asmt oc else_asmt;
      Printf.fprintf oc "%s:\n" exit_lbl
  | _ -> failwith "Expression type not implemented"

(* 3. Handle the sequence of LET assignments *)
and emit_asmt oc = function
  | LET (id, e, next) ->
      emit_exp oc e;
      push_stack id;
      let index = find_index id in
      Printf.fprintf oc "\tstr r0, [fp, #%d]\n" index;
      emit_asmt oc next
  | EXP e -> emit_exp oc e

(* 4. Handle the Toplevel Program (Functions and Main) *)
let rec emit_toplevel oc = function
  | Main asmt ->
      Printf.fprintf oc ".global main\nmain:\n";
      Printf.fprintf oc "\tpush {fp, lr}\n\tadd fp, sp, #4\n\tsub sp, sp, #128\n";
      reset_stack ();
      emit_asmt oc asmt;
      Printf.fprintf oc "\tsub sp, fp, #4\n\tpop {fp, pc}\n"
  | Function (func, rest) ->
      let (IDENT name) = func.name in
      Printf.fprintf oc ".global %s\n%s:\n" (Id.to_string name) (Id.to_string name);
      Printf.fprintf oc "\tpush {fp, lr}\n\tadd fp, sp, #4\n\tsub sp, sp, #128\n";
      reset_stack ();
      (* Save arguments from r0, r1, r2... to the stack *)
      List.iteri (fun i arg_id ->
        push_stack arg_id;
        let index = find_index arg_id in
        Printf.fprintf oc "\tstr r%d, [fp, #%d]\n" i index
      ) func.args;
      emit_asmt oc func.body;
      Printf.fprintf oc "\tsub sp, fp, #4\n\tpop {fp, pc}\n";
      emit_toplevel oc rest
  | _ -> ()