module Syntax = Core.Syntax
module Type =Core.Type
let value = ref 0 
let remember_var = ref []

let gen_var_tmp () = let var =  "tmp" ^ (string_of_int (!value)) in 
  value := !value+1;
  var

let add_let (v,before_in,after_in:(string * Type.t)*Syntax.t*Syntax.t):Syntax.t = Let (v,before_in,after_in)

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
                let after_in = Syntax.Add (Var new_var_left,Var new_var_right) in
                add_let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),add_let((new_var_right,Type.gentyp ()),(normalization_function_rec r),after_in))
                )
| Sub (l , r) -> (let new_var_left = gen_var_tmp () in 
                let new_var_right = gen_var_tmp () in
                Let ((new_var_left, Type.gentyp ()),(normalization_function_rec l),(
                  Let ((new_var_right,Type.gentyp ()),(normalization_function_rec r),
                    Sub(Var new_var_left,Var new_var_right)))))

| FNeg t -> (let new_var= gen_var_tmp () in
  Let ((new_var, Type.gentyp()),normalization_function_rec (t), FNeg (Var new_var)))

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

| Let ( v , before_in , after_in) -> Let ( v , (normalization_function_rec before_in) , (normalization_function_rec after_in)) 

| If (c , then_ , else_ ) ->let condition = normalization_function_rec(c) in 
                            let cond_name = gen_var_tmp() in 
                            add_let((cond_name,Type.Bool),condition,(If ( Var(cond_name) , (normalization_function_rec then_) , (normalization_function_rec else_) )))

| LetRec (fundef , t) -> LetRec (fundef , (normalization_function_rec t)) 
| App (e,l) -> App (normalization_function_rec(e),List.map normalization_function_rec l ) 
| Tuple t_list  -> Tuple (List.map normalization_function_rec t_list) 
| LetTuple ( l , before_in , after_in) -> LetTuple (l , (normalization_function_rec before_in) , (normalization_function_rec after_in))
| Array (l , r) ->  Array (normalization_function_rec(l) , normalization_function_rec(r)) (*to implement later*)
| Get (l , r) -> Get (normalization_function_rec(l) , normalization_function_rec(r)) (*to implement later*)
| Put (t1 , t2 , t3) -> Put (normalization_function_rec(t1) , normalization_function_rec(t2) , normalization_function_rec(t3)) (*to implement later*)
let normalization_function (syntax_t:Syntax.t) = value := 0;
  normalization_function_rec syntax_t
