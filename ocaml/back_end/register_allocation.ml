open Asml

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
          | _ -> failwith "LHS of add must be a variable" in
        let v2 = match e2 with
          | Syntax.Var i -> IDENT i
          | Syntax.Int i -> INT i  (* This specifically handles the '5' *)
          | _ -> failwith "RHS of add must be variable or integer" in
        EXP(Add(id1, v2))
        
  (* ASML specific 'sub' instruction *)
  | Syntax.Sub(e1, e2) ->
      let id1 = match e1 with Syntax.Var i -> i | _ -> failwith "LHS must be var" in
      let v2 = match e2 with Syntax.Var i -> IDENT i | Syntax.Int i -> INT i | _ -> failwith "RHS error" in
      EXP(Sub(id1, v2))

  (* Handling 'call' (Function Application) *)
  | Syntax.App(Syntax.Var f, args) ->
      let arg_ids = List.map (function Syntax.Var i -> i | _ -> failwith "Args must be vars") args in
      EXP(Call(f, arg_ids))

(* Use recursion for branches so they can handle the nested Lets shown in your error *)
  | Syntax.If(Syntax.LE(Syntax.Var id1, Syntax.Var id2), e1, e2) -> 
      EXP(If(id1, LE, IDENT id2, translate_to_asml e1, translate_to_asml e2))
  
  | Syntax.If(Syntax.Eq(Syntax.Var id1, Syntax.Var id2), e1, e2) -> 
      EXP(If(id1, EQ, IDENT id2, translate_to_asml e1, translate_to_asml e2))

  (* Ensure your Let case still uses the bind_last logic we added earlier *)
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
      (* Simple float handling: load into r0 bits, or use VFP *)
      Printf.fprintf oc "\tldr r0, =%ld @ float %f\n" (Int32.bits_of_float f) f

  | Var id -> Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id)
  
  | Add(id1, v2) -> 
        Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id1);
        load_ident oc v2 "r1"; (* Correctly handles IDENT or INT *)
        Printf.fprintf oc "\tadd r0, r0, r1\n"

  | Sub(id1, v2) -> 
      Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id1);
      load_ident oc v2 "r1";
      Printf.fprintf oc "\tsub r0, r0, r1\n"

  | FMul(id1, id2) -> 
        (* Correct Float Mult using VFP registers *)
        Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id1);
        Printf.fprintf oc "\tldr r1, [fp, #%d]\n" (get_off id2);
        Printf.fprintf oc "\tvmov s0, r0\n\tvmov s1, r1\n";
        Printf.fprintf oc "\tvmul.f32 s2, s0, s1\n";
        Printf.fprintf oc "\tvmov r0, s2\n"

  | If(id1, cond, v2, then_asmt, else_asmt) ->
        let lbl_id = string_of_int (Random.int 10000) in
        let else_lbl = "_else_" ^ lbl_id in
        let end_lbl = "_end_" ^ lbl_id in
        
        (* 1. Compare *)
        Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id1);
        load_ident oc v2 "r1";
        Printf.fprintf oc "\tcmp r0, r1\n";
        
        (* 2. Jump if condition is false *)
        let jump = match cond with EQ -> "bne" | LE -> "bgt" | GE -> "blt" | _ -> "bne" in
        Printf.fprintf oc "\t%s %s\n" jump else_lbl;
        
        (* 3. Then branch (can be multiple LETs) *)
        emit_asmt oc then_asmt; 
        Printf.fprintf oc "\tb %s\n" end_lbl;
        
        (* 4. Else branch *)
        Printf.fprintf oc "%s:\n" else_lbl;
        emit_asmt oc else_asmt;
        
        (* 5. End *)
        Printf.fprintf oc "%s:\n" end_lbl

  | _ -> ()

and emit_asmt oc = function
  | LET(id, e, next) ->
        emit_exp oc e;
        allocate id;
        (* Store the result of the expression into the variable's stack slot *)
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
  Printf.fprintf oc "\tsub sp, fp, #4\n\tpop {fp, pc}\n"