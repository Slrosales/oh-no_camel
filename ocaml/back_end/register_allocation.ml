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
  | Syntax.Add(Syntax.Var id1, Syntax.Var id2) -> EXP(Add(id1, IDENT id2))
  | Syntax.Add(Syntax.Var id1, Syntax.Int i) -> EXP(Add(id1, INT i))
  | Syntax.Sub(Syntax.Var id1, Syntax.Var id2) -> EXP(Sub(id1, IDENT id2))
  | Syntax.Sub(Syntax.Var id1, Syntax.Int i) -> EXP(Sub(id1, INT i))

  | Syntax.FMul(Syntax.Var id1, Syntax.Var id2) -> 
        EXP(FMul(id1, id2))


  | Syntax.If(Syntax.Eq(Syntax.Var id1, Syntax.Var id2), e1, e2) -> 
      EXP(If(id1, EQ, IDENT id2, translate_to_asml e1, translate_to_asml e2))
  | Syntax.If(Syntax.LE(Syntax.Var id1, Syntax.Var id2), e1, e2) -> 
      EXP(If(id1, LE, IDENT id2, translate_to_asml e1, translate_to_asml e2))

  | Syntax.Let((id, _), e1, e2) ->
        let asml_e1 = translate_to_asml e1 in
        let asml_e2 = translate_to_asml e2 in
        (match asml_e1 with
        | EXP e -> 

            LET(id, e, asml_e2)
        | LET (sub_id, sub_e, sub_next) -> 

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
      Printf.fprintf oc "\tmul r0, r0, r1\n"

  | If(id1, cond, v2, then_a, else_a) ->
      let lbl = string_of_int (Random.int 1000) in
      Printf.fprintf oc "\tldr r0, [fp, #%d]\n" (get_off id1);
      load_ident oc v2 "r1";
      Printf.fprintf oc "\tcmp r0, r1\n";
      Printf.fprintf oc "\tbne _else_%s\n" lbl;
      emit_asmt oc then_a;
      Printf.fprintf oc "\tb _end_%s\n_else_%s:\n" lbl lbl;
      emit_asmt oc else_a;
      Printf.fprintf oc "_end_%s:\n" lbl
  | _ -> ()

and emit_asmt oc = function
  | LET(id, e, next) ->
      emit_exp oc e;
      allocate id;
      Printf.fprintf oc "\tstr r0, [fp, #%d]\n" (get_off id);
      emit_asmt oc next
  | EXP e -> emit_exp oc e

let generate_assembly oc k_ast =
  let asml_asmt = translate_to_asml k_ast in
  Printf.fprintf oc ".text\n.global main\nmain:\n";
  Printf.fprintf oc "\tpush {fp, lr}\n\tadd fp, sp, #4\n\tsub sp, sp, #1024\n";
  Hashtbl.clear stack_map; stack_next := -4;
  emit_asmt oc asml_asmt;
  Printf.fprintf oc "\tsub sp, fp, #4\n\tpop {fp, pc}\n"