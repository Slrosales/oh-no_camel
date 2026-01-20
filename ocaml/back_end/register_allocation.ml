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
  | A.INT i -> Printf.fprintf oc "\tmov %s, #%d\n" reg i
  | A.IDENT id -> Printf.fprintf oc "\tldr %s, [r11, #%d] @ load %s\n" reg (get_slot id) id

let counter = ref 0

(* 1. Handle the Right-Hand Side of a 'let' or a standalone expression *)
let rec emit_exp oc = function
  | A.Ident ident -> 
      load_ident oc "r0" ident

  | A.Add (id, ident_or_imm) ->
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident_or_imm;
      Printf.fprintf oc "\tadd r0, r0, r1\n"

  | A.Sub (id, ident_or_imm) ->
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id);
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
          let current_count = !counter in
          counter := !counter + 1; 
          Printf.sprintf "%s_%d" step current_count
        in

        let step_subtraction_loop = generating_next_step "subtraction_loop" in 
        let step_end = generating_next_step "subtraction_end" in


        Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id); 
        load_ident oc "r1" ident_or_imm; 
        Printf.fprintf oc "\tmov r2, #0\n"; 

        Printf.fprintf oc "%s:\n" step_subtraction_loop;
        Printf.fprintf oc "\tcmp r0, r1\n";
        Printf.fprintf oc "\tblt %s\n" step_end; (* Branch if Less Than to end *)
        Printf.fprintf oc "\tsub r0, r0, r1\n";
        Printf.fprintf oc "\tadd r2, r2, #1\n";
        
        (* Jump back to loop *)
        Printf.fprintf oc "\tb %s\n" step_subtraction_loop;

        (* LESS THAN End block and STORE result *)
        Printf.fprintf oc "%s:\n" step_end;
<<<<<<< HEAD
        Printf.fprintf oc "\tstr r2, [r11, #%d]\n" (get_slot id);
        Printf.fprintf oc "\tmov r0, r2\n"     
=======
        Printf.fprintf oc "\t str r2, [r11, #%d]\n" (get_slot id);
        Printf.fprintf oc "\t mov r0, r2\n"     

| A.FAdd (id1, id2) ->
      Printf.printf "Evaluating FLOAT-ADDITION\n";
      (* Use load_val to handle both registers and stack (r13) *)
      load_val oc "r0" (A.IDENT id1);
      load_val oc "r1" (A.IDENT id2);
      (* Bridge to Floating Point Unit *)
      Printf.fprintf oc "\t vmov s0, r0\n";
      Printf.fprintf oc "\t vmov s1, r1\n";
      Printf.fprintf oc "\t vadd.f32 s2, s0, s1\n";
      Printf.fprintf oc "\t vmov %s, s2\n" dest_reg

  | A.FSub (id1, id2) ->
      Printf.printf "Evaluating FLOAT-SUBSTRUCTION\n";
      load_val oc "r0" (A.IDENT id1);
      load_val oc "r1" (A.IDENT id2);
      Printf.fprintf oc "\t vmov s0, r0\n";
      Printf.fprintf oc "\t vmov s1, r1\n";
      Printf.fprintf oc "\t vsub.f32 s2, s0, s1\n";
      Printf.fprintf oc "\t vmov %s, s2\n" dest_reg

  | A.FMul (id1, id2) ->
      Printf.printf "Evaluating FLOAT-MULTIPLICATION\n";
      load_val oc "r0" (A.IDENT id1);
      load_val oc "r1" (A.IDENT id2);
      Printf.fprintf oc "\t vmov s0, r0\n";
      Printf.fprintf oc "\t vmov s1, r1\n";
      Printf.fprintf oc "\t vmul.f32 s2, s0, s1\n";
      Printf.fprintf oc "\t vmov %s, s2\n" dest_reg
>>>>>>> 84e44254d19b411c24820e394c3ab605f09a2fe9

  | A.Neg id ->
      Printf.fprintf oc "\tldr r1, [r11, #%d]\n" (get_slot id);
      Printf.fprintf oc "\trsb r0, r1, #0\n"

  | A.Call (func_name, arg_list) ->
      List.iteri (fun i arg_id ->
        let reg_name = "r" ^ (string_of_int i) in
        load_ident oc reg_name (A.IDENT arg_id)
      ) arg_list;
      Printf.fprintf oc "\tbl %s\n" (Id.to_string func_name)

  | A.Callclo (function_name, arguments_list) ->
      Printf.printf "Evaluating Closure\n";

      let number_of_arguments = ref 0 in
      let function_pointer = ref 0 in

      Printf.fprintf oc "\tpush {lr}\n";

      List.iteri (fun index argument_id ->
        number_of_arguments := index - 1;
        function_pointer := ((get_slot argument_id) - (!number_of_arguments * 4));
        let assigning_register = "r" ^ (string_of_int (index+1)) in
        if index <= 3 then
            Printf.fprintf oc "\tldr %s, [r11, #%s]\n" assigning_register (string_of_int !function_pointer)
        else
            Printf.fprintf oc "\tpush {%s}\n" assigning_register
      ) arguments_list;

      (* let needed_memory_size = !number_of_arguments * 4 in

      Printf.fprintf oc "\tsub sp, sp, #%d\n" needed_memory_size; *)

      (* List.iteri (fun index argument_id ->
        let storing_register = "r" ^ (string_of_int index) in
        let offset = (string_of_int)(index * 4) in
        Printf.fprintf oc "\tstr %s, [sp, #%s]\n" storing_register offset
      ) arguments_list; *)

      Printf.fprintf oc "\tblx r1\n";
      (* Printf.fprintf oc "\tadd sp, sp, #%d\n" needed_memory_size; *)
      Printf.fprintf oc "\tpop {lr}\n";

      Printf.printf "Ending Closure\n"

  | A.IfEQ (id, ident, asmt1, asmt2) ->
      Printf.printf "Evaluating IF-EQUAL statement\n"; (*For debugging*)
      let branch_equal = "beq" in
      let branch_not_equal = "bne" in

      (*in order to distinguish an if statement for nested conditions by adding a count at the end of label*)
      (* let counter = ref 0 in ref allows storing and updating a value *)
      let generating_next_step step = 
        let current_count = !counter in
        counter := !counter + 1;
        Printf.sprintf "%s_%d" step current_count
      in

      let step_then = generating_next_step "then" in (*label to jump to the then block*)
      let step_else = generating_next_step "else" in (*label to jump to the else block*)
      let step_end = generating_next_step "end" in (*label to jump to end block*)

      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident;
      Printf.fprintf oc "\tcmp r0, r1\n";
      Printf.fprintf oc "\t%s %s\n" branch_equal step_then;
      Printf.fprintf oc "\t%s %s\n" branch_not_equal step_else;

      Printf.printf "Evaluating THEN block\n";
      Printf.fprintf oc "%s:\n" step_then;
      emit_asmt oc asmt1; (*Processing assembly instructions if the condition is correct*)
      Printf.fprintf oc "\tb %s\n" step_end;

      (* Printf.fprintf oc "\t%s %s\n" branch_equal step_else; *)
      Printf.printf "Evaluating ELSE block\n";
      Printf.fprintf oc "%s:\n" step_else;
      emit_asmt oc asmt2 ; (*Processing assembly instructions if the condition is incorrect*)
      Printf.fprintf oc "%s:\n" step_end;

  | A.IfLE (id, ident, asmt1, asmt2) ->
      Printf.printf "Evaluating IF-LESS-THAN-EQUAL statement\n";
      let branch_less_equal = "ble" in

      let generating_next_step step =
        let current_count = !counter in
        counter := !counter + 1;
        Printf.sprintf "%s_%d" step current_count
      in

      let step_then = generating_next_step "then" in
      let step_else = generating_next_step "else" in
      let step_end = generating_next_step "end" in

      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident;
      Printf.fprintf oc "\tcmp r0, r1\n";
      Printf.fprintf oc "\t%s %s\n" branch_less_equal step_then;
      Printf.fprintf oc "\tb %s\n" step_else;

      Printf.printf "Evaluating THEN block\n";
      Printf.fprintf oc "%s:\n" step_then;
      emit_asmt oc asmt1;
      Printf.fprintf oc "\tb %s\n" step_end;

      Printf.printf "Evaluating ELSE block\n";
      Printf.fprintf oc "%s:\n" step_else;
      emit_asmt oc asmt2;
      Printf.fprintf oc "%s:\n" step_end;

  | A.IfGE (id, ident, asmt1, asmt2) ->
      Printf.printf "Evaluating IF-GREATER-THAN-EQUAL statement\n";
      let branch_greater_equal = "bge" in

      let generating_next_step step =
        let current_count = !counter in
        counter := !counter + 1;
        Printf.sprintf "%s_%d" step current_count
      in
        
      let step_then = generating_next_step "then" in
      let step_else = generating_next_step "else" in
      let step_end = generating_next_step "end" in

      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident;
      Printf.fprintf oc "\tcmp r0, r1\n";
      Printf.fprintf oc "\t%s %s\n" branch_greater_equal step_then;
      Printf.fprintf oc "\tb %s\n" step_else;

      Printf.printf "Evaluating THEN block\n";
      Printf.fprintf oc "%s:\n" step_then;
      emit_asmt oc asmt1;
      Printf.fprintf oc "\tb %s\n" step_end;

      Printf.printf "Evaluating ELSE block\n";
      Printf.fprintf oc "%s:\n" step_else;
      emit_asmt oc asmt2;
      Printf.fprintf oc "%s:\n" step_end;

      (* Case: mem(arr + index) *)
  | A.MemLoad(arr_id, index_id_or_imm) ->
<<<<<<< HEAD
      (* 1. Load the base address (pointer) *)
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot arr_id);
      (* 2. Load the index into r1 *)
      load_ident oc "r1" index_id_or_imm;
      (* 3. Perform the scaled load: r0 = memory[r0 + (r1 * 4)] *)
      Printf.fprintf oc "\tldr r0, [r0, r1, lsl #2]\n"
=======
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot arr_id);
      load_ident oc "r1" index_id_or_imm;
      Printf.fprintf oc "\t ldr r0, [r0, r1, lsl #2]\n"
>>>>>>> 84e44254d19b411c24820e394c3ab605f09a2fe9


  | A.Memstore(arr_id, index_id_or_imm, value_id) ->
<<<<<<< HEAD
      (* 1. Load the base address of the array into r0 *)
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot arr_id);
      (* 2. Load the index into r1 *)
      load_ident oc "r1" index_id_or_imm;
      (* 3. Load the value to be stored into r2 *)
      Printf.fprintf oc "\tldr r2, [r11, #%d]\n" (get_slot value_id);
      (* 4. Perform the scaled store: memory[r0 + (r1 * 4)] = r2 *)
      Printf.fprintf oc "\tstr r2, [r0, r1, lsl #2]\n"
=======
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot arr_id);
      load_ident oc "r1" index_id_or_imm;
      Printf.fprintf oc "\t ldr r2, [r11, #%d]\n" (get_slot value_id);
      Printf.fprintf oc "\t str r2, [r0, r1, lsl #2]\n"
>>>>>>> 84e44254d19b411c24820e394c3ab605f09a2fe9

  (* Case: new size *)
  | A.New(size_id_or_imm) ->
      load_ident oc "r0" size_id_or_imm;
<<<<<<< HEAD
      (* 2. Convert elements to bytes (elements * 4) *)
      Printf.fprintf oc "\tlsl r0, r0, #2\n";
      (* 3. Call your heap allocator *)
      Printf.fprintf oc "\tbl _min_caml_alloc\n"
=======
      Printf.fprintf oc "\t lsl r0, r0, #2\n";
      Printf.fprintf oc "\t bl _min_caml_alloc\n"
>>>>>>> 84e44254d19b411c24820e394c3ab605f09a2fe9


  | _ -> () (*Ignore other types for now to avoid crashes*)

and emit_asmt oc = function
  | A.LET (id, exp, next_asmt) ->
      Printf.printf "Evaluating LET\n";
      emit_exp oc exp;
      let offset = get_slot id in
      Printf.fprintf oc "\tstr r0, [r11, #%d] @ store result %s\n" offset id;
      emit_asmt oc next_asmt
  | A.EXP exp ->
      Printf.printf "Evaluating EXP\n";
      emit_exp oc exp

let rec generate_assembly oc prog =
  match prog with
  | A.Function (func, next_inst) ->
<<<<<<< HEAD
      reset_stack ();
      Printf.fprintf oc ".text\n.global %s\n\n%s:\n" func.name func.name;
    
      Printf.fprintf oc "\tpush {r11, r14}\n";
      Printf.fprintf oc "\tmov r11, r13\n";
      Printf.fprintf oc "\tsub r13, r13, #16 @ reqserve frame\n";

      List.iteri (fun i param_id ->
        if i < 4 then
          let offset = get_slot param_id in
          Printf.fprintf oc "\tstr r%d, [r11, #%d] @ save param %s \n" i offset param_id
=======

      Part_1.allocate_registers func.body func.args;
      
      Printf.fprintf oc ".text\n.global %s\n%s:\n" func.name func.name;

      Printf.fprintf oc "\t push {r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #4\n";    (* r11 now points to saved r14 *)
      Printf.fprintf oc "\t sub r13, r13, #1024\n"; 


      List.iteri (fun i param_id ->
        if i < 4 then
          match Part_1.get_loc param_id with
          | Part_1.Stack offset -> 
              Printf.fprintf oc "\t str r%d, [r11, #%d] @ save param %s\n" i offset param_id
          | Part_1.Reg r -> 
              Printf.fprintf oc "\t mov %s, r%d @ move param %s to home reg\n" r i param_id
>>>>>>> 84e44254d19b411c24820e394c3ab605f09a2fe9
      ) func.args;

      emit_asmt oc func.body;      

<<<<<<< HEAD
      Printf.fprintf oc "\tadd r13, r13, #16\n";
      Printf.fprintf oc "\tpop {r11, r15}\n\n";
=======

      Printf.fprintf oc "\t sub r13, r11, #4\n";
      Printf.fprintf oc "\t pop {r11, r14}\n";
      Printf.fprintf oc "\t bx r14\n"; 
>>>>>>> 84e44254d19b411c24820e394c3ab605f09a2fe9
      
      generate_assembly oc next_inst

  | A.Main asmt ->
<<<<<<< HEAD
      reset_stack ();
      Printf.fprintf oc ".text\n.global _start\n\n _start:\n";

      (* Prologue *)
      Printf.fprintf oc "\tpush {r11, r14}\n";
      Printf.fprintf oc "\tadd r11, r13, #4\n";
      Printf.fprintf oc "\tsub r13, r13, #2048\n";

      emit_asmt oc asmt;

      (* For line after the ouput *)
      Printf.fprintf oc "\tbl _min_caml_print_newline\n";

      (* Epilogue *)
      Printf.fprintf oc "\tsub r13, r11, #4\n";
      Printf.fprintf oc "\tpop {r11, r14}\n";


      (* Exit Branch *)
      Printf.fprintf oc "\tbl	_min_caml_exit\n"

=======
      Part_1.allocate_registers asmt [];
      Printf.fprintf oc ".text \n.global _start\n\n_start:\n";


      Printf.fprintf oc "\t push {r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #4\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";

      emit_asmt oc asmt;


      Printf.fprintf oc "\t bl _min_caml_print_newline\n";
      Printf.fprintf oc "\t sub r13, r11, #4\n";
      Printf.fprintf oc "\t pop {r11, r14}\n";
      Printf.fprintf oc "\t bl _min_caml_exit\n"

  | A.Nil -> ()
>>>>>>> 84e44254d19b411c24820e394c3ab605f09a2fe9
