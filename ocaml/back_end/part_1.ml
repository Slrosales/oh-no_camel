module A = Core.Asml
module Id = Core.Id

type location = Reg of string | Stack of int


let safe_local_regs = ["r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"; "r12"]
let allocation_map = Hashtbl.create 128
let stack_count = ref 0

let reset_allocation () =
  Hashtbl.clear allocation_map;
  stack_count := 0

let allocate_registers asmt args =
  reset_allocation ();
  let free_regs = ref safe_local_regs in

  (* To assign a stable location to an identifier *)
  let assign_location id =
    if not (Hashtbl.mem allocation_map id) then (
      match !free_regs with
      | r :: rest -> 
          Hashtbl.replace allocation_map id (Reg r); 
          free_regs := rest
      | [] -> 
          incr stack_count; 
          Hashtbl.replace allocation_map id (Stack (!stack_count * 4))
    )
  in

  List.iter assign_location args;


  let rec scan = function
    | A.LET (id, _, next) ->
        assign_location id;
        scan next
    | A.EXP (A.IfEQ (_, _, a1, a2))
    | A.EXP (A.IfLE (_, _, a1, a2))
    | A.EXP (A.IfGE (_, _, a1, a2)) -> 
        scan a1; 
        scan a2
    | A.EXP _ -> ()
  in 
  scan asmt

let get_loc id = 
  try Hashtbl.find allocation_map id 
  with Not_found -> Reg "r0"