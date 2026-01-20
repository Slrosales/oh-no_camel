(* module A = Core.Asml
module Id = Core.Id


let reg_pool = ["r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"]
let reg_usage = Hashtbl.create 7

let reset_regs () = 
  Hashtbl.clear reg_usage;
  List.iter (fun r -> Hashtbl.add reg_usage r false) reg_pool

let get_free_reg () =
  try List.find (fun r -> not (Hashtbl.find reg_usage r)) reg_pool
  with Not_found -> "r0" 
let occupy r = if r <> "r0" then Hashtbl.replace reg_usage r true
let release r = if r <> "r0" then Hashtbl.replace reg_usage r false

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
  | A.IDENT id -> Printf.fprintf oc "\t ldr %s, [r11, #%d] @ load %s\n" reg (get_slot id) id

let global_label_count = ref 0
let gen_label prefix =
  let c = !global_label_count in
  global_label_count := c + 1;
  Printf.sprintf "%s_%d" prefix c


let rec emit_exp oc = function
  | A.Ident ident -> 
      load_ident oc "r0" ident

  | A.Add (id, ident_or_imm) ->
      let r1 = get_free_reg () in occupy r1;
      let r2 = get_free_reg () in occupy r2;
      Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" r1 (get_slot id);
      load_ident oc r2 ident_or_imm;
      Printf.fprintf oc "\t add r0, %s, %s\n" r1 r2;
      release r1; release r2

  | A.Sub (id, ident_or_imm) ->
      let r1 = get_free_reg () in occupy r1;
      let r2 = get_free_reg () in occupy r2;
      Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" r1 (get_slot id);
      load_ident oc r2 ident_or_imm;
      Printf.fprintf oc "\t sub r0, %s, %s\n" r1 r2;
      release r1; release r2

  | A.Mul (id, ident_or_imm) ->
      let r1 = get_free_reg () in occupy r1;
      let r2 = get_free_reg () in occupy r2;
      Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" r1 (get_slot id);
      load_ident oc r2 ident_or_imm;
      Printf.fprintf oc "\t mul r0, %s, %s\n" r1 r2;
      release r1; release r2

  | A.Div (id, ident_or_imm) ->
      let loop = gen_label "div_loop" in
      let finish = gen_label "div_end" in
      let r1 = get_free_reg () in occupy r1;
      let r2 = get_free_reg () in occupy r2;
      Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" r1 (get_slot id); 
      load_ident oc r2 ident_or_imm; 
      Printf.fprintf oc "\t mov r3, #0\n"; 
      Printf.fprintf oc "%s:\n" loop;
      Printf.fprintf oc "\t cmp %s, %s\n" r1 r2;
      Printf.fprintf oc "\t blt %s\n" finish;
      Printf.fprintf oc "\t sub %s, %s, %s\n" r1 r1 r2;
      Printf.fprintf oc "\t add r3, r3, #1\n";
      Printf.fprintf oc "\t b %s\n" loop;
      Printf.fprintf oc "%s:\n" finish;
      Printf.fprintf oc "\t mov r0, r3\n";
      release r1; release r2

  | A.Neg id ->
      let r1 = get_free_reg () in
      Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" r1 (get_slot id);
      Printf.fprintf oc "\t rsb r0, %s, #0\n" r1

  | A.Call (func_name, arg_list) ->
      List.iteri (fun i arg_id ->
        if i < 4 then load_ident oc ("r" ^ string_of_int i) (A.IDENT arg_id)
      ) arg_list;
      Printf.fprintf oc "\t bl %s\n" (Id.to_string func_name)

  | A.IfEQ (id, ident, asmt1, asmt2) -> emit_if oc "beq" id ident asmt1 asmt2
  | A.IfLE (id, ident, asmt1, asmt2) -> emit_if oc "ble" id ident asmt1 asmt2
  | A.IfGE (id, ident, asmt1, asmt2) -> emit_if oc "bge" id ident asmt1 asmt2

  | A.MemLoad(arr_id, index_id_or_imm) ->
      let r_base = get_free_reg () in occupy r_base;
      let r_idx = get_free_reg () in
      Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" r_base (get_slot arr_id);
      load_ident oc r_idx index_id_or_imm;
      Printf.fprintf oc "\t ldr r0, [%s, %s, lsl #2]\n" r_base r_idx;
      release r_base

  | A.Memstore(arr_id, index_id_or_imm, value_id) ->
      let r_base = get_free_reg () in occupy r_base;
      let r_idx = get_free_reg () in occupy r_idx;
      let r_val = get_free_reg () in
      Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" r_base (get_slot arr_id);
      load_ident oc r_idx index_id_or_imm;
      Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" r_val (get_slot value_id);
      Printf.fprintf oc "\t str %s, [%s, %s, lsl #2]\n" r_val r_base r_idx;
      release r_base; release r_idx

  | A.New(size_id_or_imm) ->
      load_ident oc "r0" size_id_or_imm;
      Printf.fprintf oc "\t lsl r0, r0, #2\n";
      Printf.fprintf oc "\t bl _min_caml_alloc\n"
  | _ -> ()

and emit_if oc cond id ident asmt1 asmt2 =
  let label_then = gen_label "then" in
  let label_end = gen_label "else_end" in
  let r1 = get_free_reg () in occupy r1;
  let r2 = get_free_reg () in
  Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" r1 (get_slot id);
  load_ident oc r2 ident;
  Printf.fprintf oc "\t cmp %s, %s\n" r1 r2;
  release r1;
  Printf.fprintf oc "\t %s %s\n" cond label_then;
  emit_asmt oc asmt2;
  Printf.fprintf oc "\t b %s\n" label_end;
  Printf.fprintf oc "%s:\n" label_then;
  emit_asmt oc asmt1;
  Printf.fprintf oc "%s:\n" label_end;

and emit_asmt oc = function
  | A.LET (id, exp, next_asmt) ->
      emit_exp oc exp;
      Printf.fprintf oc "\t str r0, [r11, #%d] @ store result %s\n" (get_slot id) id;
      emit_asmt oc next_asmt
  | A.EXP exp ->
      emit_exp oc exp

let rec generate_assembly oc = function
  | A.Function (func, next_inst) ->
      reset_stack (); reset_regs ();
      Printf.fprintf oc "\n.global %s\n%s:\n" func.name func.name;
      Printf.fprintf oc "\t push {r4-r10, r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #28\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";
      List.iteri (fun i p -> if i < 4 then Printf.fprintf oc "\t str r%d, [r11, #%d]\n" i (get_slot p)) func.args;
      emit_asmt oc func.body;      
      Printf.fprintf oc "\t sub r13, r11, #28\n";
      Printf.fprintf oc "\t pop {r4-r10, r11, r14}\n\t bx lr\n";
      generate_assembly oc next_inst

  | A.Main asmt ->
      reset_stack (); reset_regs ();
      Printf.fprintf oc "\n.text\n.global _start\n_start:\n";
      Printf.fprintf oc "\t push {r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #4\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";
      emit_asmt oc asmt;
      Printf.fprintf oc "\t bl _min_caml_print_newline\n";
      Printf.fprintf oc "\t sub r13, r11, #4\n";
      Printf.fprintf oc "\t pop {r11, r14}\n\t bl _min_caml_exit\n"
  | _ -> () *)


(* module A = Core.Asml
module Id = Core.Id


let load_val oc target_reg = function
  | A.INT i -> Printf.fprintf oc "\t mov %s, #%d\n" target_reg i
  | A.IDENT id ->
      (match Part_1.get_loc id with
      | Part_1.Reg r -> if r <> target_reg then Printf.fprintf oc "\t mov %s, %s\n" target_reg r
      | Part_1.Stack off -> Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" target_reg off)

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
      let r_lhs = (match Part_1.get_loc left_val with 
        | Part_1.Reg r -> r 
        | Part_1.Stack off -> 
            Printf.fprintf oc "\t ldr r2, [r11, #%d]\n" off; 
            "r2") in
      let r_rhs = (match right_val with 
        | A.INT i -> 
            Printf.fprintf oc "\t mov r3, #%d\n" i; 
            "r3" 
        | A.IDENT id_rhs -> 
            (match Part_1.get_loc id_rhs with 
             | Part_1.Reg r -> r 
             | Part_1.Stack off -> 
                 Printf.fprintf oc "\t ldr r3, [r11, #%d]\n" off; 
                 "r3")) in
      Printf.fprintf oc "\t %s %s, %s, %s\n" op_code dest_reg r_lhs r_rhs

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

  | A.Call (func_name, args) ->
      List.iteri (fun i arg -> 
        if i < 4 then 
          load_val oc (Printf.sprintf "r%d" i) (A.IDENT arg)
      ) args;
      Printf.fprintf oc "\t bl %s\n" (Id.to_string func_name);
      if dest_reg <> "r0" then 
        Printf.fprintf oc "\t mov %s, r0\n" dest_reg

  | A.MemLoad (array, index) ->
      load_val oc "r0" (A.IDENT array);
      load_val oc "r1" index;
      Printf.fprintf oc "\t ldr %s, [r0, r1, lsl #2]\n" dest_reg

  | A.Memstore (array, index, value) ->
      load_val oc "r0" (A.IDENT array);
      load_val oc "r1" index;
      load_val oc "r2" (A.IDENT value);
      Printf.fprintf oc "\t str r2, [r0, r1, lsl #2]\n"

  | A.New (array_size) ->
      load_val oc "r0" array_size;
      Printf.fprintf oc "\t lsl r0, r0, #2\n";
      Printf.fprintf oc "\t bl _min_caml_alloc\n";
      if dest_reg <> "r0" then 
        Printf.fprintf oc "\t mov %s, r0\n" dest_reg

  | _ -> () 

and emit_if oc cond id ident asmt1 asmt2 =
  let l_then, l_end = gen_label "then", gen_label "else_end" in
  let r_lhs = (match Part_1.get_loc id with 
    | Part_1.Reg r -> r 
    | Part_1.Stack off -> Printf.fprintf oc "\t ldr r2, [r11, #%d]\n" off; "r2") in
  let r_rhs = (match ident with 
    | A.INT i -> Printf.fprintf oc "\t mov r3, #%d\n" i; "r3" 
    | A.IDENT id_rhs -> 
        (match Part_1.get_loc id_rhs with 
        | Part_1.Reg r -> r 
        | Part_1.Stack off -> Printf.fprintf oc "\t ldr r3, [r11, #%d]\n" off; "r3")) in
  Printf.fprintf oc "\t cmp %s, %s\n" r_lhs r_rhs;
  Printf.fprintf oc "\t %s %s\n" cond l_then;
  emit_asmt oc asmt2;
  Printf.fprintf oc "\t b %s\n" l_end;
  Printf.fprintf oc "%s:\n" l_then;
  emit_asmt oc asmt1;
  Printf.fprintf oc "%s:\n" l_end

and emit_asmt oc asmt = 
  match asmt with
  | A.LET (id, exp, next) ->
      (match Part_1.get_loc id with
       | Part_1.Reg r -> 
           emit_exp oc r exp
       | Part_1.Stack off -> 
           emit_exp oc "r0" exp; 
           Printf.fprintf oc "\t str r0, [r11, #%d]\n" off);
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
      Printf.fprintf oc "\t push {r4-r10, r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #32\n"; 
      Printf.fprintf oc "\t sub r13, r13, #1024\n"; 

      List.iteri (fun i p -> 
        match Part_1.get_loc p with
        | Part_1.Reg r -> 
            if i < 4 && r <> ("r"^string_of_int i) then 
              Printf.fprintf oc "\t mov %s, r%d\n" r i
        | Part_1.Stack off -> 
            if i < 4 then 
              Printf.fprintf oc "\t str r%d, [r11, #%d]\n" i off
      ) func.args;

      emit_asmt oc func.body;      
      Printf.fprintf oc "\t sub r13, r11, #32\n";
      Printf.fprintf oc "\t pop {r4-r10, r11, r15}\n";
      generate_assembly oc next_inst

  | A.Main asmt ->
      Part_1.allocate_registers asmt [];
      Printf.fprintf oc "\n.text\n.global _start\n_start:\n";
      Printf.fprintf oc "\t push {r4-r10, r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #32\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";
      emit_asmt oc asmt;
      Printf.fprintf oc "\t bl _min_caml_print_newline\n";
      Printf.fprintf oc "\t sub r13, r11, #32\n";
      Printf.fprintf oc "\t pop {r4-r10, r11, r14}\n";
      Printf.fprintf oc "\t bl _min_caml_exit\n"

  | _ -> () *)






module A = Core.Asml
module Id = Core.Id


let load_val oc target_reg = function
  | A.INT i -> Printf.fprintf oc "\t mov %s, #%d\n" target_reg i
  | A.IDENT id ->
      (match Part_1.get_loc id with
      | Part_1.Reg r -> if r <> target_reg then Printf.fprintf oc "\t mov %s, %s\n" target_reg r
      | Part_1.Stack off -> Printf.fprintf oc "\t ldr %s, [r11, #%d]\n" target_reg off)

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
      let r_lhs = (match Part_1.get_loc left_val with 
        | Part_1.Reg r -> r 
        | Part_1.Stack off -> 
            Printf.fprintf oc "\t ldr r2, [r11, #%d]\n" off; 
            "r2") in
      let r_rhs = (match right_val with 
        | A.INT i -> 
            Printf.fprintf oc "\t mov r3, #%d\n" i; 
            "r3" 
        | A.IDENT id_rhs -> 
            (match Part_1.get_loc id_rhs with 
             | Part_1.Reg r -> r 
             | Part_1.Stack off -> 
                 Printf.fprintf oc "\t ldr r3, [r11, #%d]\n" off; 
                 "r3")) in
      Printf.fprintf oc "\t %s %s, %s, %s\n" op_code dest_reg r_lhs r_rhs

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
        let r_val = (match Part_1.get_loc id with
          | Part_1.Reg r -> r
          | Part_1.Stack off -> 
              Printf.fprintf oc "\t ldr r1, [r13, #%d]\n" off;
              "r1") in
        Printf.fprintf oc "\t rsb %s, %s, #0\n" dest_reg r_val


  | A.Call (func_name, args) ->
          List.iteri (fun i arg -> 
            if i < 4 then load_val oc (Printf.sprintf "r%d" i) (A.IDENT arg)
          ) args;
          List.iteri (fun i arg ->
            if i >= 4 then (
              load_val oc "r3" (A.IDENT arg);
              let spill_offset = (i - 4) * 4 in
              Printf.fprintf oc "\t str r3, [r13, #%d]\n" spill_offset
            )
          ) args;
          Printf.fprintf oc "\t bl %s\n" (Id.to_string func_name);
          if dest_reg <> "r0" then 
            Printf.fprintf oc "\t mov %s, r0\n" dest_reg

  (* | A.Callclo (function_name, arguments) ->
      Printf.printf "Evaluating Closure\n";

      let number_of_arguments = ref 0 in
      let function_pointer = ref 0 in

      Printf.fprintf oc "\tpush {lr}\n";

      List.iteri (fun index argument ->
        number_of_arguments := index - 1;
        function_pointer := ((get_slot argument) - (number_of_arguments * 4));
        Printf.fprintf oc ""
        ) *)

  | A.MemLoad (array, index) ->
      load_val oc "r0" (A.IDENT array);
      load_val oc "r1" index;
      Printf.fprintf oc "\t ldr %s, [r0, r1, lsl #2]\n" dest_reg

  | A.Memstore (array, index, value) ->
      load_val oc "r0" (A.IDENT array);
      load_val oc "r1" index;
      load_val oc "r2" (A.IDENT value);
      Printf.fprintf oc "\t str r2, [r0, r1, lsl #2]\n"

  | A.New (array_size) ->
      load_val oc "r0" array_size;
      Printf.fprintf oc "\t lsl r0, r0, #2\n";
      Printf.fprintf oc "\t bl _min_caml_alloc\n";
      if dest_reg <> "r0" then 
        Printf.fprintf oc "\t mov %s, r0\n" dest_reg

  | _ -> () 

and emit_if oc cond id ident asmt1 asmt2 =
  let l_then, l_end = gen_label "then", gen_label "else_end" in
  let r_lhs = (match Part_1.get_loc id with 
    | Part_1.Reg r -> r 
    | Part_1.Stack off -> Printf.fprintf oc "\t ldr r2, [r11, #%d]\n" off; "r2") in
  let r_rhs = (match ident with 
    | A.INT i -> Printf.fprintf oc "\t mov r3, #%d\n" i; "r3" 
    | A.IDENT id_rhs -> 
        (match Part_1.get_loc id_rhs with 
        | Part_1.Reg r -> r 
        | Part_1.Stack off -> Printf.fprintf oc "\t ldr r3, [r11, #%d]\n" off; "r3")) in
  Printf.fprintf oc "\t cmp %s, %s\n" r_lhs r_rhs;
  Printf.fprintf oc "\t %s %s\n" cond l_then;
  emit_asmt oc asmt2;
  Printf.fprintf oc "\t b %s\n" l_end;
  Printf.fprintf oc "%s:\n" l_then;
  emit_asmt oc asmt1;
  Printf.fprintf oc "%s:\n" l_end

and emit_asmt oc asmt = 
  match asmt with
  | A.LET (id, exp, next) ->
      (match Part_1.get_loc id with
       | Part_1.Reg r -> 
           emit_exp oc r exp
       | Part_1.Stack off -> 
           emit_exp oc "r0" exp; 
           Printf.fprintf oc "\t str r0, [r11, #%d]\n" off);
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
      Printf.fprintf oc "\t push {r4-r10, r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #32\n"; 
      Printf.fprintf oc "\t sub r13, r13, #1024\n"; 

      List.iteri (fun i p -> 
        match Part_1.get_loc p with
        | Part_1.Reg r -> 
            if i < 4 && r <> ("r"^string_of_int i) then 
              Printf.fprintf oc "\t mov %s, r%d\n" r i
        | Part_1.Stack off -> 
            if i < 4 then 
              Printf.fprintf oc "\t str r%d, [r11, #%d]\n" i off
      ) func.args;

      emit_asmt oc func.body;      
      Printf.fprintf oc "\t sub r13, r11, #32\n";
      Printf.fprintf oc "\t pop {r4-r10, r11, r15}\n";
      generate_assembly oc next_inst

  | A.Main asmt ->
      Part_1.allocate_registers asmt [];
      Printf.fprintf oc "\n.text\n.global _start\n_start:\n";
      Printf.fprintf oc "\t push {r4-r10, r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #32\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";
      emit_asmt oc asmt;
      Printf.fprintf oc "\t bl _min_caml_print_newline\n";
      Printf.fprintf oc "\t sub r13, r11, #32\n";
      Printf.fprintf oc "\t pop {r4-r10, r11, r14}\n";
      Printf.fprintf oc "\t bl _min_caml_exit\n"

  | _ -> ()