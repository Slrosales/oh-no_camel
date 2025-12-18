(*This test was made with the help of AI*)
(* Compile with: ocamlc -o test_id syntax.cmo type.cmo id.cmo id_test.ml *)

(* 1. Helper for assertions *)
let assert_equal msg v1 v2 =
  if v1 <> v2 then (
    Printf.eprintf "[FAIL] %s: Expected '%s', got '%s'\n" msg v2 v1;
    exit 1
  ) else (
    Printf.printf "[PASS] %s\n" msg
  )

let assert_neq msg v1 v2 =
  if v1 = v2 then (
    Printf.eprintf "[FAIL] %s: Expected different values, got '%s' == '%s'\n" msg v1 v2;
    exit 1
  ) else (
    Printf.printf "[PASS] %s\n" msg
  )

(* 2. Test Uniqueness *)
module StringSet = Set.Make(String)

let test_uniqueness count =
  Printf.printf "--- Testing Uniqueness on %d calls ---\n" count;
  let rec loop i set =
    if i = 0 then set
    else
      let new_id = Id.genid () in
      if StringSet.mem new_id set then (
        Printf.eprintf "[FAIL] Duplicate ID generated: %s\n" new_id;
        exit 1
      ) else
        loop (i - 1) (StringSet.add new_id set)
  in
  let final_set = loop count StringSet.empty in
  Printf.printf "[PASS] Generated %d unique IDs without collision.\n" (StringSet.cardinal final_set)

(* 3. Test Compiler Simulation *)
let test_compiler_simulation () =
  Printf.printf "--- Simulating Compiler Usage ---\n";

  (* Scenario A: Parser encounters '_' or ';' *)
  let temp_var_1 = Id.gentmp Type.Unit in
  Printf.printf "  [Parser] Generated temp for wildcard: %s\n" temp_var_1;

  (* Scenario B: K-Normalization generates a temporary for calculation *)
  let temp_var_2 = Id.gentmp Type.Int in
  Printf.printf "  [K-Norm] Generated temp for (1+2): %s\n" temp_var_2;
  
  assert_neq "Temps must be unique" temp_var_1 temp_var_2;

  (* Scenario C: Backend uses Labels *)
  (* IMPORTANT: Labels must preserve the exact string you give them! *)
  let func_name = "min_caml_print_int" in
  let label = Id.L func_name in
  
  (* We need to unpack the label to check it *)
  let extracted_name = match label with Id.L s -> s in
  assert_equal "Label must preserve function name" extracted_name func_name;

  (* Scenario D: Verify Skeleton Logic (Types are ignored) *)
  (* In the original MinCaml, these would be different prefixes (Ti vs Td). 
     In your Skeleton, they should look the same (vN vs vM). *)
  let v_int = Id.gentmp Type.Int in
  let v_float = Id.gentmp Type.Float in
  
  Printf.printf "  [Check] Int ID: %s, Float ID: %s\n" v_int v_float;
  
  if v_int.[0] = v_float.[0] then
    Printf.printf "[PASS] Skeleton Behavior Confirmed: Types share the same prefix '%c'\n" v_int.[0]
  else
    Printf.printf "[WARN] Types have different prefixes. Did you change id.ml?\n"

(* 4. Main Runner *)
let () =
  Printf.printf "=== Running Id Tests ===\n";
  
  (* Basic check *)
  let v1 = Id.genid () in
  let v2 = Id.genid () in
  assert_neq "Sequential calls" v1 v2;

  (* Simulation *)
  test_compiler_simulation ();

  (* Stress Test *)
  test_uniqueness 1000;
  
  Printf.printf "=== All Tests Passed ===\n"