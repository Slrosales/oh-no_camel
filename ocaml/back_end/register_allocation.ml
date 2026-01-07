(* open Asml

let rec concat_asmt a1 a2 =
  match a1 with
  | EXP e -> 

      LET(Id.genid (), e, a2)
  | LET(id, e, next) -> 
      LET(id, e, concat_asmt next a2)

let rec translate_to_asml = function
  | Syntax.Unit -> EXP(NOP)
  | Syntax.Int i -> EXP(Int i)
  | Syntax.Var id -> EXP(Var id)

  | Syntax.Add(e1, e2) ->
        let id1 = match e1 with 
          | Syntax.Var i -> i 
          | _ -> failwith "LHS of add intruction must be a variable" in
        let v2 = match e2 with
          | Syntax.Var i -> IDENT i
          | Syntax.Int i -> INT i 
          | _ -> failwith "RHS of add instruction must be variable or integer" in
        EXP(Add(id1, v2))
        

  | Syntax.Sub(e1, e2) ->
      let id1 = match e1 with Syntax.Var i -> i | _ -> failwith "LHS must be var" in
      let v2 = match e2 with Syntax.Var i -> IDENT i | Syntax.Int i -> INT i | _ -> failwith "RHS error" in
      EXP(Sub(id1, v2))

  | Syntax.App(Syntax.Var "call", [Syntax.Var func; Syntax.Var arg]) ->
      EXP(Call(func, [arg]))

  | Syntax.App(Syntax.Var f, args) ->
      let arg_ids = List.map (function Syntax.Var i -> i | _ -> failwith "Args must be vars") args in
      EXP(Call(f, arg_ids))

  | Syntax.Let(("_", _), e1, e2) ->
      let asml_e1 = translate_to_asml e1 in
      let asml_e2 = translate_to_asml e2 in
      (match asml_e1 with
      | EXP e -> concat_asmt (EXP e) asml_e2
      | LET _ as a -> concat_asmt a asml_e2)


  | Syntax.If(Syntax.LE(Syntax.Var id1, Syntax.Var id2), e1, e2) -> 
      EXP(If(id1, LE, IDENT id2, translate_to_asml e1, translate_to_asml e2))
  
  | Syntax.If(Syntax.Eq(Syntax.Var id1, Syntax.Var id2), e1, e2) -> 
      EXP(If(id1, EQ, IDENT id2, translate_to_asml e1, translate_to_asml e2))

  | Syntax.Let((id, _), e1, e2) ->
      let asml_e1 = translate_to_asml e1 in
      let asml_e2 = translate_to_asml e2 in
      (match asml_e1 with
       | EXP e -> LET(id, e, asml_e2)
       | LET _ -> 
           let rec bind_last = function
             | EXP e -> LET(id, e, asml_e2)
             | LET(i, e, n) -> LET(i, e, bind_last n)
           in
           bind_last asml_e1)


  (* Failure statement that will be replaced later *)
  | e -> failwith ("Unsupported Syntax: " ^ (Syntax.to_string e))

  
let stack_map = Hashtbl.create 50 
let stack_next = ref (-4)

let allocate id = 
  if not (Hashtbl.mem stack_map id) then (
    Hashtbl.add stack_map id !stack_next;
    stack_next := !stack_next - 4 
  )

let get_off id = 
  try Hashtbl.find stack_map id
  with Not_found -> failwith ("Variable not found: " ^ id)

let load_ident oc v reg = match v with
  | INT i -> Printf.fprintf oc "\tmov %s, #%d\n" reg i
  | IDENT id -> Printf.fprintf oc "\tldr %s, [fp, #%d]\n" reg (get_off id)


let rec emit_exp oc = function

  | Int i -> Printf.fprintf oc "\tmov r0, #%d\n" i

  | Float f -> 

      Printf.fprintf oc "\tldr r0, =%ld @ float %f\n" (Int32.bits_of_float f) f

  | Var id -> Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id)
  
  | Add(id1, v2) -> 
        Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id1);
        load_ident oc v2 "r1"; 
        Printf.fprintf oc "\tadd r0, r0, r1\n"

  | Sub(id1, v2) -> 
      Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id1);
      load_ident oc v2 "r1";
      Printf.fprintf oc "\tsub r0, r0, r1\n"

  | FMul(id1, id2) -> 
  
        Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id1);
        Printf.fprintf oc "\tldr r1, [fp, #%d]\n" (get_off id2);
        Printf.fprintf oc "\tvmov s0, r0\n\tvmov s1, r1\n";
        Printf.fprintf oc "\tvmul.f32 s2, s0, s1\n";
        Printf.fprintf oc "\tvmov r0, s2\n"

  | If(id1, cond, v2, then_asmt, else_asmt) ->
        let label_id = string_of_int (Random.int 10000) in
        let else_lbl = "_else_" ^ label_id in
        let end_lbl = "_end_" ^ label_id in
        

        Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id1);
        load_ident oc v2 "r1";
        Printf.fprintf oc "\tcmp r0, r1\n";
  
        let jump = match cond with EQ -> "bne" | LE -> "bgt" | GE -> "blt" | _ -> "bne" in
        Printf.fprintf oc "\t%s %s\n" jump else_lbl;
        

        emit_asmt oc then_asmt; 
        Printf.fprintf oc "\tb %s\n" end_lbl;
        

        Printf.fprintf oc "%s:\n" else_lbl;
        emit_asmt oc else_asmt;
        

        Printf.fprintf oc "%s:\n" end_lbl


  | Call(func_name, arg_ids) ->
      List.iteri (fun i arg_id ->
        if i < 4 then
          let reg = "r" ^ (string_of_int i) in
          Printf.fprintf oc "\tldr %s, [fp, #%d]\n" reg (get_off arg_id)
      ) arg_ids;
      Printf.fprintf oc "\tbl %s\n" func_name

  | _ -> ()

and emit_asmt oc = function
  | LET(id, e, next) ->
        emit_exp oc e;
        allocate id;

        Printf.fprintf oc "\tstr r0, [fp, #%d]\n" (get_off id);
        emit_asmt oc next
    | EXP e -> 
        emit_exp oc e

let generate_assembly oc k_ast =
  let asml_asmt = translate_to_asml k_ast in
  Printf.fprintf oc ".text\n.global main\nmain:\n";
  Printf.fprintf oc "\tpush {fp, lr}\n\tadd fp, sp, #4\n\tsub sp, sp, #1024\n";
  Hashtbl.clear stack_map; stack_next := -4;
  emit_asmt oc asml_asmt;
  Printf.fprintf oc "\tsub sp, fp, #4\n\tpop {fp, pc}\n" *)


module K = K_normalization


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



let rec emit_exp oc = function
  | K.Unit -> ()
  | K.Int i -> Printf.fprintf oc "\tmov r0, #%d\n" i
  | K.Var x -> 
      let offset = get_slot x in
      Printf.fprintf oc "\tldr r0, [r11, #%d] @ load %s\n" offset x


  | K.App ("add", [x; y]) ->
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot x);
      Printf.fprintf oc "\tldr r1, [r11, #%d]\n" (get_slot y);
      Printf.fprintf oc "\tadd r0, r0, r1\n"

  | K.App ("sub", [x; y]) ->
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot x);
      Printf.fprintf oc "\tldr r1, [r11, #%d]\n" (get_slot y);
      Printf.fprintf oc "\tsub r0, r0, r1\n"

  | K.Neg x ->
      Printf.fprintf oc "\tldr r0, [r11, #%d]\n" (get_slot x);
      Printf.fprintf oc "\trsb r0, r0, #0\n"


  | K.App (f_name, args) ->

      let target_label = match f_name with
        | "print_int"     -> "_min_caml_print_int"
        | "print_newline" -> "_min_caml_print_newline"
        | "print_char"    -> "_min_caml_print_char"
        | "exit"          -> "_min_caml_exit"
        | other           -> other
      in
      
      List.iteri (fun i arg_id ->
        if i < 4 then
          let offset = get_slot arg_id in
          Printf.fprintf oc "\tldr r%d, [r11, #%d] @ arg %d\n" i offset i
      ) args;
      
      Printf.fprintf oc "\tbl %s\n" target_label

  | K.IfEq (x, y, e1, e2) ->

      ()

  | K.IfLE (x, y, e1, e2) ->

      ()

  | K.Let ((id, _), e1, e2) ->
      emit_exp oc e1;
      let offset = get_slot id in
      Printf.fprintf oc "\tstr r0, [r11, #%d] @ store %s\n" offset id;
      emit_exp oc e2


  | other -> 
      Printf.fprintf oc "\t@ TODO: Implement pattern for %s\n" (K.to_string other)



let generate_assembly oc (k_exp : K.t) =
  reset_stack ();
  Printf.fprintf oc ".text\n.align 2\n.global main\n\nmain:\n";
  
  (* Prologue *)
  (* r11 = fp, r13 = sp, r14 = lr *)
  Printf.fprintf oc "\tpush {r11, r14}     @ save fp and lr\n";
  Printf.fprintf oc "\tadd r11, r13, #4    @ set fp (r11) to previous sp\n";
  Printf.fprintf oc "\tsub r13, r13, #1024 @ allocate stack space (sp = r13)\n";
  
  emit_exp oc k_exp;
  
  (* pilogue *)
  (* r15 = pc *)
  Printf.fprintf oc "\tsub r13, r11, #4    @ restore sp (r13)\n";
  Printf.fprintf oc "\tpop {r11, r15}      @ restore fp and jump to lr (pc = r15)\n"