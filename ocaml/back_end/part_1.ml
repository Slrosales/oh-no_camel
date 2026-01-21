(* module A = Core.Asml
module Id = Core.Id

type location = Reg of string | Stack of int

let all_regs = ["r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"]
let allocation_map = Hashtbl.create 128
let stack_count = ref 0

let reset_allocation () =
  Hashtbl.clear allocation_map;
  stack_count := 0

let allocate_registers asmt args =
  reset_allocation ();
  let free_regs = ref all_regs in
  
  (* ARM calling convention: first 4 args in r0-r3 *)
  List.iteri (fun i arg ->
    if i < 4 then 
      Hashtbl.replace allocation_map arg (Reg ("r" ^ string_of_int i))
    else (
      incr stack_count;
      Hashtbl.replace allocation_map arg (Stack (!stack_count * -4)))
  ) args;

  let rec scan = function
    | A.LET (id, _, next) ->
        (* Use replace instead of add to handle potential re-definitions *)
        if not (Hashtbl.mem allocation_map id) then (
          match !free_regs with
          | r :: rest -> 
              Hashtbl.replace allocation_map id (Reg r); 
              free_regs := rest
          | [] -> 
              incr stack_count; 
              Hashtbl.replace allocation_map id (Stack (!stack_count * -4))
        );
        scan next
    | A.EXP _ -> ()
  in scan asmt

let get_loc id = 
  try Hashtbl.find allocation_map id 
  with Not_found -> 
    (* Debugging: helps find variables that weren't allocated *)
    Printf.printf "Warning: %s not found in map\n" id;
    Stack 0

(* Updated for 8-byte alignment *)
let get_stack_size () = 
  let raw_size = !stack_count * 4 in
  if raw_size mod 8 = 0 then raw_size else raw_size + 4 *)


(* 
module A = Core.Asml
module Id = Core.Id

type location = Reg of string | Stack of int

let all_regs = ["r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"]
let allocation_map = Hashtbl.create 128
let stack_count = ref 0

let reset_allocation () =
  Hashtbl.clear allocation_map;
  stack_count := 0

let allocate_registers asmt args =
  reset_allocation ();
  let free_regs = ref all_regs in
  

  List.iteri (fun i arg ->
    if i < 4 then 
      Hashtbl.replace allocation_map arg (Reg ("r" ^ string_of_int i))
    else (
      incr stack_count;
      Hashtbl.replace allocation_map arg (Stack (i * 4)) 
    )
  ) args;

  let rec scan = function
    | A.LET (id, _, next) ->
        if not (Hashtbl.mem allocation_map id) then (
          match !free_regs with
          | r :: rest -> 
              Hashtbl.replace allocation_map id (Reg r); 
              free_regs := rest
          | [] -> 
              incr stack_count; 
              Hashtbl.replace allocation_map id (Stack (!stack_count * -4 - 32))
        );
        scan next
    | A.EXP (A.IfEQ (_, _, a1, a2))
    | A.EXP (A.IfLE (_, _, a1, a2))
    | A.EXP (A.IfGE (_, _, a1, a2)) -> scan a1; scan a2
    | A.EXP _ -> ()
  in scan asmt

let get_loc id = 
  try Hashtbl.find allocation_map id with Not_found -> Reg "r0"

let get_stack_size () = 
  let raw = !stack_count * 4 in
  (raw + 7) land (lnot 7)

 *)


module A = Core.Asml
module Id = Core.Id

type location = Reg of string | Stack of int

let all_regs = ["r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"]
let allocation_map = Hashtbl.create 128
let stack_count = ref 0

let reset_allocation () =
  Hashtbl.clear allocation_map;
  stack_count := 0

  let allocate_registers asmt args =
    reset_allocation () ;
    let free_regs = ref all_regs in
    List.iteri (fun i arg ->
      if i < 4 then
        Hashtbl.replace allocation_map arg (Reg ("r" ^ string_of_int i))
      else
        (* 1024 (buffer) + 36 (pushed regs) = 1060. 
          Arg 5 is at [sp, #1060], Arg 6 at [sp, #1064], etc. *)
        let extra_offset = (*1060 +*) ((i - 4) * 4 + 4) in
        Hashtbl.replace allocation_map arg (Stack extra_offset)
    ) args ;



  let rec scan = function
    | A.LET (id, _, next) ->
        if not (Hashtbl.mem allocation_map id) then (
          match !free_regs with
          | r :: rest -> 
              Hashtbl.replace allocation_map id (Reg r); 
              free_regs := rest
          | [] -> 
              incr stack_count; 
              Hashtbl.replace allocation_map id (Stack (!stack_count * 4))
        );
        scan next
    | A.EXP (A.IfEQ (_, _, a1, a2))
    | A.EXP (A.IfLE (_, _, a1, a2))
    | A.EXP (A.IfGE (_, _, a1, a2)) -> scan a1; scan a2
    | A.EXP _ -> ()
  in scan asmt

let get_loc id = try Hashtbl.find allocation_map id with Not_found -> Reg "r0"
let get_stack_size () = (!stack_count * 4 + 7) land (lnot 7)