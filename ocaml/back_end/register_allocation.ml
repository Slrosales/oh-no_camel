(* module A = Core.Asml
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

let counter = ref 0

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
          let current_count = !counter in
          counter := !counter + 1; 
          Printf.sprintf "%s_%d" step current_count
        in

        let step_subtraction_loop = generating_next_step "subtraction_loop" in 
        let step_end = generating_next_step "subtraction_end" in


        Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id); 
        load_ident oc "r1" ident_or_imm; 
        Printf.fprintf oc "\t mov r2, #0\n"; 

        Printf.fprintf oc "%s:\n" step_subtraction_loop;
        Printf.fprintf oc "\t cmp r0, r1\n";
        Printf.fprintf oc "\t blt %s\n" step_end; (* Branch if Less Than to end *)
        Printf.fprintf oc "\t sub r0, r0, r1\n";
        Printf.fprintf oc "\t add r2, r2, #1\n";
        
        (* Jump back to loop *)
        Printf.fprintf oc "\t b %s\n" step_subtraction_loop;

        (* LESS THAN End block and STORE result *)
        Printf.fprintf oc "%s:\n" step_end;
        Printf.fprintf oc "\t str r2, [r11, #%d]\n" (get_slot id);
        Printf.fprintf oc "\t mov r0, r2\n"     


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


      (* Case: mem(arr + index) *)
  | A.MemLoad(arr_id, index_id_or_imm) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot arr_id);
      load_ident oc "r1" index_id_or_imm;
      Printf.fprintf oc "\t ldr r0, [r0, r1, lsl #2]\n"


  | A.Memstore(arr_id, index_id_or_imm, value_id) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot arr_id);
      load_ident oc "r1" index_id_or_imm;
      Printf.fprintf oc "\t ldr r2, [r11, #%d]\n" (get_slot value_id);
      Printf.fprintf oc "\t str r2, [r0, r1, lsl #2]\n"

  (* Case: new size *)
  | A.New(size_id_or_imm) ->
      load_ident oc "r0" size_id_or_imm;
      Printf.fprintf oc "\t lsl r0, r0, #2\n";
      Printf.fprintf oc "\t bl _min_caml_alloc\n"


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

let rec generate_assembly oc prog =
  match prog with
  | A.Function (func, next_inst) ->

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
      ) func.args;

      emit_asmt oc func.body;      


      Printf.fprintf oc "\t sub r13, r11, #4\n";
      Printf.fprintf oc "\t pop {r11, r14}\n";
      Printf.fprintf oc "\t bx r14\n"; 
      
      generate_assembly oc next_inst

  | A.Main asmt ->
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

  | A.Nil -> () *)


  module A = Core.Asml
module Id = Core.Id


let stack_map = Hashtbl.create 128
let stack_next = ref (-12) 

let get_slot id =
  match Hashtbl.find_opt stack_map id with
  | Some offset -> offset
  | None ->
      let offset = !stack_next in
      Hashtbl.add stack_map id offset;
      stack_next := !stack_next - 4;
      offset

let reset_stack () =
  Hashtbl.clear stack_map;
  stack_next := -12


let load_ident oc reg = function
  | A.INT i -> Printf.fprintf oc "\t mov %s, #%d\n" reg i
  | A.IDENT id -> 
      let offset = get_slot id in
      Printf.fprintf oc "\t ldr %s, [r11, #%d] @ load %s\n" reg offset (Id.to_string id)

let counter = ref 0
let gen_label prefix =
  let c = !counter in
  counter := c + 1;
  Printf.sprintf "_%s_%d" prefix c

let rec emit_exp oc = function
  | A.Ident ident -> load_ident oc "r0" ident

  | A.Add (id, ident_or_imm) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident_or_imm;
      Printf.fprintf oc "\t add r0, r0, r1\n"

  | A.Sub (id, ident_or_imm) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident_or_imm;
      Printf.fprintf oc "\t sub r0, r0, r1\n"

  | A.Mul (id, ident_or_imm) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident_or_imm;
      Printf.fprintf oc "\t mul r0, r0, r1\n"

  | A.Neg id ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id);
      Printf.fprintf oc "\t rsb r0, r0, #0\n"

  | A.Call (func_name, arg_list) ->
      List.iteri (fun i arg_id ->
        if i < 4 then load_ident oc (Printf.sprintf "r%d" i) (A.IDENT arg_id)
      ) arg_list;
      Printf.fprintf oc "\t bl %s\n" (Id.to_string func_name)

  | A.IfEQ (id, ident, asmt1, asmt2) | A.IfLE (id, ident, asmt1, asmt2) | A.IfGE (id, ident, asmt1, asmt2) as if_type ->
      let label_else = gen_label "else" in
      let label_end = gen_label "end" in
      let branch_instr = match if_type with
        | A.IfEQ _ -> "bne"
        | A.IfLE _ -> "bgt"
        | A.IfGE _ -> "blt"
        | _ -> "bne"
      in
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident;
      Printf.fprintf oc "\t cmp r0, r1\n";
      Printf.fprintf oc "\t %s %s\n" branch_instr label_else;
      emit_asmt oc asmt1;
      Printf.fprintf oc "\t b %s\n%s:\n" label_end label_else;
      emit_asmt oc asmt2;
      Printf.fprintf oc "%s:\n" label_end

  | A.MemLoad(arr_id, index_id_or_imm) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot arr_id);
      load_ident oc "r1" index_id_or_imm;
      Printf.fprintf oc "\t ldr r0, [r0, r1, lsl #2]\n"

  | A.Memstore(arr_id, index_id_or_imm, val_id) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot arr_id);
      load_ident oc "r1" index_id_or_imm;
      Printf.fprintf oc "\t ldr r2, [r11, #%d]\n" (get_slot val_id);
      Printf.fprintf oc "\t str r2, [r0, r1, lsl #2]\n"

  | A.New(size_imm_or_id) ->
      load_ident oc "r0" size_imm_or_id;
      Printf.fprintf oc "\t lsl r0, r0, #2\n";
      Printf.fprintf oc "\t bl _min_caml_alloc\n"

  | _ -> ()


and emit_asmt oc = function
  | A.LET (id, exp, next) ->
      emit_exp oc exp;
      let offset = get_slot id in
      Printf.fprintf oc "\t str r0, [r11, #%d] @ store result %s\n" offset (Id.to_string id);
      emit_asmt oc next
  | A.EXP exp ->
      emit_exp oc exp


let rec generate_assembly oc prog =
  match prog with
  | A.Function (func, next_inst) ->
      reset_stack ();
      Printf.fprintf oc "\n.global %s\n%s:\n" func.name func.name;
      Printf.fprintf oc "\t push {r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #4\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";


      List.iteri (fun i p ->
        if i < 4 then 
          Printf.fprintf oc "\t str r%d, [r11, #%d] @ param %s\n" i (get_slot p) (Id.to_string p)
      ) func.args;

      emit_asmt oc func.body;

      Printf.fprintf oc "\t sub r13, r11, #4\n";
      Printf.fprintf oc "\t pop {r11, r14}\n";
      Printf.fprintf oc "\t bx r14\n";
      generate_assembly oc next_inst

  | A.Main asmt ->
      reset_stack ();
      Printf.fprintf oc "\n.text\n.global _start\n_start:\n";
      Printf.fprintf oc "\t push {r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #4\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";
      
      emit_asmt oc asmt;
      
      Printf.fprintf oc "\t bl _min_caml_print_newline\n";
      Printf.fprintf oc "\t sub r13, r11, #4\n";
      Printf.fprintf oc "\t pop {r11, r14}\n";
      Printf.fprintf oc "\t bl _min_caml_exit\n"

  | _ -> ()