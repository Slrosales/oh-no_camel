module A = Asml

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


let emit_exp oc = function
  | A.Ident ident -> 
      load_ident oc "r0" ident

  | A.Add (id, ident) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident;
      Printf.fprintf oc "\t add r0, r0, r1\n"

  | A.Sub (id, ident) ->
      Printf.fprintf oc "\t ldr r0, [r11, #%d]\n" (get_slot id);
      load_ident oc "r1" ident;
      Printf.fprintf oc "\t sub r0, r0, r1\n"

  | A.Call (func_name, arg_list) ->
      List.iteri (fun i arg_id ->
        let reg_name = "r" ^ (string_of_int i) in
        load_ident oc reg_name (A.IDENT arg_id)
      ) arg_list;
      Printf.fprintf oc "\t bl %s\n" (Id.to_string func_name)

  | _ -> () (* Operation not implemented *)


let rec emit_asmt oc = function
  | A.LET (id, exp, next_asmt) ->
      emit_exp oc exp;
      let offset = get_slot id in
      Printf.fprintf oc "\t str r0, [r11, #%d] @ store result %s\n" offset id;
      emit_asmt oc next_asmt
  | A.EXP exp ->
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
      Printf.fprintf oc "\t pop {r11, r15}\n";
      
      generate_assembly oc next_inst

  | A.Main asmt ->
      reset_stack ();
      Printf.fprintf oc ".text \n.global main\n\n main:\n";

      (* Prologue *)
      Printf.fprintf oc "\t push {r11, r14}\n";
      Printf.fprintf oc "\t add r11, r13, #4\n";
      Printf.fprintf oc "\t sub r13, r13, #1024\n";

      emit_asmt oc asmt;

      (* Epilogue *)
      Printf.fprintf oc "\t sub r13, r11, #4\n";
      Printf.fprintf oc "\t pop {r11, r15}\n"
