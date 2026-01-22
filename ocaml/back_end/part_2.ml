module A = Core.Asml
module Id = Core.Id

(* Helper to load a value  into a specific ARM register *)
let load_val oc target_reg = function
  | A.INT i -> 
      Printf.fprintf oc "\t mov %s, #%d\n" target_reg i
  | A.IDENT id ->
      (match Part_1.get_loc id with
      | Part_1.Reg r -> 
          if r <> target_reg then Printf.fprintf oc "\t mov %s, %s\n" target_reg r
      | Part_1.Stack off -> 
          (* Local variables are stored below the frame pointer r11 *)
          Printf.fprintf oc "\t ldr %s, [r11, #-%d]\n" target_reg off)

let global_label_count = ref 0
let gen_label prefix =
  let c = !global_label_count in
  global_label_count := c + 1;
  Printf.sprintf "%s_%d" prefix c

let rec emit_exp oc dest_reg exp = 
  match exp with
  | A.Ident ident -> 
      load_val oc dest_reg ident

  | A.Add (left_val, right_val)
  | A.Sub (left_val, right_val)
  | A.Mul (left_val, right_val) ->
      let op_code = match exp with 
        | A.Add _ -> "add" 
        | A.Sub _ -> "sub" 
        | _ -> "mul" in
      (* r1 and r2 are trasitory registers to avoid overwriting local variable registers r4-r12 *)
      load_val oc "r1" (A.IDENT left_val);
      load_val oc "r2" right_val;
      Printf.fprintf oc "\t %s %s, r1, r2\n" op_code dest_reg

  | A.Div (dividend, divisor) ->
      let loop = gen_label "div_loop" in
      let finish = gen_label "div_end" in
      load_val oc "r2" (A.IDENT dividend); 
      load_val oc "r3" divisor;
      Printf.fprintf oc "\t mov r1, #0\n";
      Printf.fprintf oc "%s:\n" loop;
      Printf.fprintf oc "\t cmp r2, r3\n";
      Printf.fprintf oc "\t blt %s\n" finish;
      Printf.fprintf oc "\t sub r2, r2, r3\n";
      Printf.fprintf oc "\t add r1, r1, #1\n";
      Printf.fprintf oc "\t b %s\n" loop;
      Printf.fprintf oc "%s:\n" finish;
      Printf.fprintf oc "\t mov %s, r1\n" dest_reg

  | A.Neg id ->
        load_val oc "r1" (A.IDENT id);
        Printf.fprintf oc "\t rsb %s, r1, #0\n" dest_reg

  | A.Call (func_name, args) ->
          (* Arguments r0-r3 as per ARM convention *)
          List.iteri (fun i arg -> 
            if i < 4 then load_val oc (Printf.sprintf "r%d" i) (A.IDENT arg)
          ) args;
          (* Arguments 5+ on Stack if when the r0-r3 get full *)
          List.iteri (fun i arg ->
            if i >= 4 then (
              load_val oc "r12" (A.IDENT arg);
              let spill_offset = (i - 4) * 4 in
              Printf.fprintf oc "\t str r12, [r13, #%d]\n" spill_offset
            )
          ) args;
          Printf.fprintf oc "\t bl %s\n" (Id.to_string func_name);

          if dest_reg <> "r0" then 
            Printf.fprintf oc "\t mov %s, r0\n" dest_reg

  | A.MemLoad (array, index) ->
      load_val oc "r1" (A.IDENT array);
      load_val oc "r2" index;
      Printf.fprintf oc "\t ldr %s, [r1, r2, lsl #2]\n" dest_reg

  | A.Memstore (array, index, value) ->
      load_val oc "r1" (A.IDENT array);
      load_val oc "r2" index;
      load_val oc "r3" (A.IDENT value);
      Printf.fprintf oc "\t str r3, [r1, r2, lsl #2]\n"

  | A.New (array_size) ->
      load_val oc "r0" array_size;
      Printf.fprintf oc "\t lsl r0, r0, #2\n";
      Printf.fprintf oc "\t bl _min_caml_alloc\n";
      if dest_reg <> "r0" then 
        Printf.fprintf oc "\t mov %s, r0\n" dest_reg

  | _ -> () 

and emit_if oc condition l_operand r_operand inst_true inst_false =
  let l_then, l_end = gen_label "then", gen_label "else_end" in
  load_val oc "r1" (A.IDENT l_operand);
  load_val oc "r2" r_operand;
  Printf.fprintf oc "\t cmp r1, r2\n";
  Printf.fprintf oc "\t %s %s\n" condition l_then;
  emit_asmt oc inst_false;
  Printf.fprintf oc "\t b %s\n" l_end;
  Printf.fprintf oc "%s:\n" l_then;
  emit_asmt oc inst_true;
  Printf.fprintf oc "%s:\n" l_end

and emit_asmt oc asmt = 
  match asmt with
  | A.LET (id, exp, next) ->
      (match Part_1.get_loc id with
       | Part_1.Reg r -> emit_exp oc r exp
       | Part_1.Stack off -> 
           emit_exp oc "r0" exp; 
           Printf.fprintf oc "\t str r0, [r11, #-%d]\n" off);
      emit_asmt oc next
  | A.EXP (A.IfEQ (id, ident, a1, a2)) -> emit_if oc "beq" id ident a1 a2
  | A.EXP (A.IfLE (id, ident, a1, a2)) -> emit_if oc "ble" id ident a1 a2
  | A.EXP (A.IfGE (id, ident, a1, a2)) -> emit_if oc "bge" id ident a1 a2
  | A.EXP e -> 
      emit_exp oc "r0" e

let rec generate_assembly oc prog =
  match prog with
  | A.Function (func, next_inst) ->
      Part_1.allocate_registers func.body func.args;
      Printf.fprintf oc "\n.global %s\n%s:\n" func.name func.name;
      
      (* PROLOGUE *)
      Printf.fprintf oc "\t push {r4-r11, r12, r14}\n";
      Printf.fprintf oc "\t mov r11, r13\n"; 
      Printf.fprintf oc "\t sub r13, r13, #1024\n"; (* Local workspace *)

      (* Move incoming parameters from r0-r3 into safe r4-r12 *)
      List.iteri (fun i p -> 
        let loc = Part_1.get_loc p in
        match loc with
        | Part_1.Reg target_reg ->
            if i < 4 then
              let src_reg = "r" ^ string_of_int i in
              if target_reg <> src_reg then 
                Printf.fprintf oc "\t mov %s, %s\n" target_reg src_reg
            else
              (* Parameter 5+ is on the stack *)
              let caller_stack_off = (i - 4) * 4 + 40 in
              Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" target_reg caller_stack_off
        | Part_1.Stack off -> 
            if i < 4 then
              Printf.fprintf oc "\t str r%d, [r11, #-%d]\n" i off
      ) func.args;

      emit_asmt oc func.body;      

      (* EPILOGUE *)
      Printf.fprintf oc "\t mov r13, r11\n";
      Printf.fprintf oc "\t pop {r4-r11, r12, r15}\n";
      generate_assembly oc next_inst

  | A.Main asmt ->
      Part_1.allocate_registers asmt [];
      Printf.fprintf oc "\n.text\n.global _start\n_start:\n";
      Printf.fprintf oc "\t push {r4-r11, r12, r14}\n";
      Printf.fprintf oc "\t mov r11, r13\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";

      emit_asmt oc asmt;

      Printf.fprintf oc "\t bl _min_caml_print_newline\n";
      Printf.fprintf oc "\t mov r13, r11\n";
      Printf.fprintf oc "\t pop {r4-r11, r12, r14}\n";
      Printf.fprintf oc "\t bl _min_caml_exit\n"

  | _ -> ()