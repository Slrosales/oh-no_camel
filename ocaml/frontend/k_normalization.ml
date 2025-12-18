module Syntax = Core.Syntax
module Type =Core.Type
let value = ref 0 

let gen_var_tmp () = let var =  "tmp" ^ (string_of_int (!value)) in 
  value := !value+1;
  var

let is_a_base_case (t:Syntax.t):bool = match t with
| Unit -> true
| Int _ -> true
| Bool _ -> true
| Float _ -> true
| _ -> false


let rec normalization_function_rec (syntax_t:Syntax.t):Syntax.t = match syntax_t with
(**base case*)
| Unit -> Unit
| Bool b -> Bool b
| Int i -> Int i
| Float f -> Float f
| Var id -> Var id (*to implement later*)
(*recusive case*)
| Not t -> Not t (*to implement later*)
| Neg t -> Neg t (*to implement later*)
| Add (l,r) -> (let new_var_left = gen_var_tmp () in 
                let new_var_right = gen_var_tmp () in
                Let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),(
                  Let ((new_var_right,Type.gentyp ()),(normalization_function_rec r),
                    Add(Var new_var_left,Var new_var_right)))))
| Sub (l , r) -> (let new_var_left = gen_var_tmp () in 
                let new_var_right = gen_var_tmp () in
                Let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),(
                  Let ((new_var_right,Type.gentyp ()),(normalization_function_rec r),
                    Sub(Var new_var_left,Var new_var_right)))))
| FNeg t -> FNeg (normalization_function_rec (t)) (*to implement later*)
| FAdd (l , r) -> (let new_var_left = gen_var_tmp () in 
                let new_var_right = gen_var_tmp () in
                Let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),(
                  Let ((new_var_right,Type.gentyp ()),(normalization_function_rec r),
                    FAdd(Var new_var_left,Var new_var_right)))))
| FSub (l,r) -> (let new_var_left = gen_var_tmp () in 
                let new_var_right = gen_var_tmp () in
                Let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),(
                  Let ((new_var_right,Type.gentyp ()),(normalization_function_rec r),
                    FSub(Var new_var_left,Var new_var_right)))))
| FMul (l , r) -> (let new_var_left = gen_var_tmp () in 
                let new_var_right = gen_var_tmp () in
                Let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),(
                  Let ((new_var_right,Type.gentyp ()),(normalization_function_rec r),
                    FMul(Var new_var_left,Var new_var_right))))) 
| FDiv (l , r) -> (let new_var_left = gen_var_tmp () in 
                let new_var_right = gen_var_tmp () in
                Let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),(
                  Let ((new_var_right,Type.gentyp ()),(normalization_function_rec r),
                    FDiv(Var new_var_left,Var new_var_right))))) 
| Eq (l , r) -> (let new_var_left = gen_var_tmp () in 
                let new_var_right = gen_var_tmp () in
                Let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),(
                  Let ((new_var_right,Type.gentyp ()),(normalization_function_rec r),
                    Eq(Var new_var_left,Var new_var_right)))))
| LE (l , r) -> (let new_var_left = gen_var_tmp () in 
                let new_var_right = gen_var_tmp () in
                Let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),(
                  Let ((new_var_right,Type.gentyp ()),(normalization_function_rec r),
                    LE(Var new_var_left,Var new_var_right)))))
| If (c , then_ , else_ ) -> If (c , (normalization_function_rec then_) , (normalization_function_rec else_) ) (*to be implemented later*)
| Let ( v , before_in , after_in) -> Let ( v , (normalization_function_rec before_in) , (normalization_function_rec after_in)) 

(*TODO Harde one to do *)
| LetRec (fundef , t) -> LetRec (fundef , (normalization_function_rec t)) (*to implement later*)
| App (e,l) -> App (e,l) (*to implement later*)
| Tuple t_list  -> Tuple t_list (*to implement later*)
| LetTuple ( l , t1 , t2) -> LetTuple (l , t1 , t2) (*to implement later*)
| Array (l , r) ->  Array (l , r) (*to implement later*)
| Get (l , r) -> Get (l , r) (*to implement later*)
| Put (t1 , t2 , t3) -> Put (t1 , t2 , t3) (*to implement later*)
let normalization_function (syntax_t:Syntax.t) = value := 0;
  normalization_function_rec syntax_t
