module A = Core.Asml
module Id = Core.Id

let stack_map = Hashtbl.create 128
let stack_next = ref (-12) 

let get_slot id =
  match Hashtbl.find_opt stack_map id with
  | Some offset -> 
      Printf.printf "Slot for %s: %d\n" (Id.to_string id) offset;
      offset
  | None ->
      let offset = !stack_next in
      Hashtbl.add stack_map id offset;
      stack_next := !stack_next - 4;
      offset

let reset_stack () =
  Hashtbl.clear stack_map;
  stack_next := -12

(* Helper to load constants or variables into a specific register *)
let load_ident oc reg = function
  | A.INT i -> Printf.fprintf oc "\t mov %s, #%d\n" reg i
  | A.IDENT id -> Printf.fprintf oc "\t ldr %s, [r11, #%d] @ load %s\n" reg (get_slot id) id

(* 1. Handle the Right-Hand Side of a 'let' or a standalone expression *)
let rec emit_exp oc = function
  | A.Ident ident -> 
      load_ident oc "r0" ident

  | A.Add (id, ident_or_imm) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident_or_imm;
      Printf.fprintf oc "\tadd r0, r0, r1\n"

  | A.Sub (id, ident_or_imm) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident_or_imm;
      Printf.fprintf oc "\tsub r0, r0, r1\n"

  | A.Mul (id, ident_or_imm) ->
      Printf.printf "Evaluating MULTIPLICATION\n";
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident_or_imm;
      Printf.fprintf oc "\tmul r0, r0, r1\n"

  | A.Div (id, ident_or_imm) ->
      Printf.printf "Evaluating DIVISION\n";

      let generating_next_step step =
          Printf.sprintf "%s" step;
      in

      let step_subtraction_loop = generating_next_step "subtraction loop" in 
      let step_end = generating_next_step "end" in
      let branch_greater_equal = "bge" in

      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id); (*Loading dividend to r1*)
      load_ident oc "r1" ident_or_imm; (*Loading divisor to r0*)
      Printf.fprintf oc "\tmov r2, #0\n"; (*r2 for the result*)
      
      (*Keep subtracting until the divisor is greater than the dividend*)
      Printf.fprintf oc "%s:" step_subtraction_loop;
      Printf.fprintf oc "\tcmp r1, r0\n";
      Printf.fprintf oc "\t%s %s\n" branch_greater_equal step_end;
      Printf.fprintf oc "\tsub r0, r0, r1\n";
      Printf.fprintf oc "\tadd r2, r2, #1\n"; (*The result is incremented at each loop*)
      Printf.fprintf oc "\tb %s" step_subtraction_loop;

      (*End block, after the loop is terminated*)
      Printf.fprintf oc "%s:\n" step_end

  | A.FAdd (id1, id2) ->
      Printf.printf "Evaluating FLOAT-ADDITION\n";
      Printf.fprintf oc "tldr r0, [r11, #%d]\n" (get_slot id1);
      Printf.fprintf oc "tldr r1, [r11, #%d]\n" (get_slot id2);
      Printf.fprintf oc "\tfadd r0, r0, r1\n"

  | A.FSub (id1, id2) ->
      Printf.printf "Evaluating FLOAT-SUBSTRUCTION\n";
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id1);
      Printf.fprintf oc "\tldr r1, [r11, #%d]\n" (get_slot id2);
      Printf.fprintf oc "\tfsub r0, r0, r1\n"

  | A.FMul (id1, id2) ->
      Printf.printf "Evaluating FLOAT-MULTIPLICATION\n";
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id1);
      Printf.fprintf oc "\tldr r1, [r11, #%d]\n" (get_slot id2);
      Printf.fprintf oc "\tfmul r0, r0, r1\n"

  | A.Neg id ->
      Printf.fprintf oc "\tldr r1, [r11, #%d]\n" (get_slot id);
      Printf.fprintf oc "\trsb r0, r1, #0\n"

  | A.Call (func_name, arg_list) ->
      List.iteri (fun i arg_id ->
        let reg_name = "r" ^ (string_of_int i) in
        load_ident oc reg_name (A.IDENT arg_id)
      ) arg_list;
      Printf.fprintf oc "\t bl %s\n" (Id.to_string func_name)

  | A.IfEQ (id, ident, asmt1, asmt2) ->
      Printf.printf "Evaluating IF-EQUAL statement\n"; (*For debugging*)
      let branch_equal = "beq" in
      let branch = branch_equal in

      (*in order to distinguish an if statement for nested conditions by adding a count at the end of label*)
      let counter = ref 0 in (*ref allows storing and updating a value*)
      let generating_next_step step = 
        let current_count = !counter in
        counter := !counter + 1;
        Printf.sprintf "%s_%d" step current_count
      in

      let step_then = generating_next_step "then" in (*label to jump to the then block*)
      let step_else = generating_next_step "else" in (*label to jump to the else block*)
      let step_end = generating_next_step "end" in (*label to jump to end block*)

      Printf.fprintf oc "\tldr r1, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident;
      Printf.fprintf oc "\tcmp r0, r1\n";

      Printf.printf "Evaluating THEN block\n";
      Printf.fprintf oc "\t%s %s\n" branch step_then;
      emit_asmt oc asmt1; (*Processing assembly instructions if the condition is correct*)
      Printf.fprintf oc "\tb %s" step_end;

      Printf.printf "Evaluating ELSE blok\n";
      Printf.fprintf oc "\t%s %s\n" branch step_else;
      emit_asmt oc asmt2; (*Processing assembly instructions if the condition is incorrect*)
      Printf.fprintf oc "%s:\n" step_end;

  | A.IfLE (id, ident, asmt1, asmt2) ->
      Printf.printf "Evaluating IF-LESS-THAN-EQUAL statement\n";
      let branch_less_equal = "ble" in
      let branch = branch_less_equal in

      let counter = ref 0 in
      let generating_next_step step =
        let current_count = !counter in
        counter := !counter + 1;
        Printf.sprintf "%s_%d" step current_count
      in

      let step_then = generating_next_step "then" in
      let step_else = generating_next_step "else" in
      let step_end = generating_next_step "end" in

      Printf.fprintf oc "\tldr r1, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident;
      Printf.fprintf oc "\tcomp r0, r1\n";

      Printf.printf "Evaluating THEN block\n";
      Printf.fprintf oc "\t%s %s\n" branch step_then;
      emit_asmt oc asmt1;
      Printf.fprintf oc "\tb %s" step_end;

      Printf.printf "Evaluating ELSE block\n";
      Printf.fprintf oc "\t%s %s\n" branch step_else;
      emit_asmt oc asmt2;
      Printf.fprintf oc "%s:\n" step_end;

  | A.IfGE (id, ident, asmt1, asmt2) ->
      Printf.printf "Evaluating IF-GREATER-THAN-EQUAL statement\n";
      let branch_greater_equal = "bge" in
      let branch = branch_greater_equal in

      let counter = ref 0 in
      let generating_next_step step =
        let current_count = !counter in
        counter := !counter + 1;
        Printf.sprintf "%s_%d" step current_count
      in
        
      let step_then = generating_next_step "then" in
      let step_else = generating_next_step "else" in
      let step_end = generating_next_step "end" in

      Printf.fprintf oc "\tldr r1, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident;
      Printf.fprintf oc "\tcmp r0, r1\n";

      Printf.printf "Evaluating THEN block\n";
      Printf.fprintf oc "\t%s %s\n" branch step_then;
      emit_asmt oc asmt1;
      Printf.fprintf oc "\tb %s\n" step_end;

      Printf.printf "Evaluating ELSE block\n";
      Printf.fprintf oc "\t%s %s\n" branch step_else;
      emit_asmt oc asmt2;
      Printf.fprintf oc "%s:\n" step_end;

  | _ -> () (*Ignore other types for now to avoid crashes*)

and emit_asmt oc = function
  | A.LET (id, exp, next_asmt) ->
      Printf.printf "Evaluating LET\n";
      emit_exp oc exp;
      let offset = get_slot id in
      Printf.fprintf oc "\t str r0, [r11, #%d] @ store result %s\n" offset id;
      emit_asmt oc next_asmt
  | A.EXP exp ->
      Printf.printf "Evaluating EXP\n";
      emit_exp oc exp


let rec generate_assembly oc = function
  | A.Function (func, next_inst) ->
      reset_stack ();
      Printf.fprintf oc ".text\n %s\n\n %s:\n" func.name func.name;
    
      Printf.fprintf oc "\t push {r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #4\n";
      Printf.fprintf oc "\t sub r13, r13, #1024 @ reserve frame\n";

      List.iteri (fun i param_id ->
        if i < 4 then
          let offset = get_slot param_id in
          Printf.fprintf oc "\t str r%d, [r11, #%d] @ save param %s \n" i offset param_id
      ) func.args;

      emit_asmt oc func.body;      

      Printf.fprintf oc "\t sub r13, r11, #4\n";
      Printf.fprintf oc "\t pop {r11, r14}\n";
      
      generate_assembly oc next_inst

  | A.Main asmt ->
      reset_stack ();
      Printf.fprintf oc ".text \n.global _start\n\n _start:\n";

      (* Prologue *)
      Printf.fprintf oc "\t push {r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #4\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";

      emit_asmt oc asmt;

      (* Epilogue *)
      Printf.fprintf oc "\t sub r13, r11, #4\n";
      Printf.fprintf oc "\t pop {r11, r14}\n"
