module Syntax = Core.Syntax
module Type =Core.Type

module IntOrd = struct
  type t = int
  let compare = compare 
end

module IntSet = Set.Make(IntOrd)
let value = ref 0 

let remember_var = ref IntSet.empty
let rec gen_var_tmp () = match !value with
| i when (IntSet.mem i !remember_var) -> value := !value+1;gen_var_tmp ()
| i -> let var =  "tmp" ^ (string_of_int i) in value := !value+1;var

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
| Array (l , r) ->  Array (normalization_function_rec(l) , normalization_function_rec(r)) 
| Get (l , r) -> Get (normalization_function_rec(l) , normalization_function_rec(r))
| Put (t1 , t2 , t3) -> Put (normalization_function_rec(t1) , normalization_function_rec(t2) , normalization_function_rec(t3)) 

let rec get_int_string string start end_idx:string = match start with
| start when start>=end_idx -> ""
| start ->String.make 1 (String.get string start) ^ (get_int_string string (start+1) end_idx) 

let get_tmp_var(id:string*Type.t)=match id with
| (string,_)when (String.starts_with ~prefix:"tmp" string) -> let string_length = String.length string in 
                                                      let number = int_of_string_opt (get_int_string string 3 string_length) in
                                                      (match number with
                                                      | None -> ()
                                                      | Some s -> remember_var:= IntSet.add s !remember_var) 
| _ -> ()
let rec get_tmp_var_rec (syntax_t:Syntax.t):unit = match syntax_t with
| Unit -> ()
| Bool _ -> () 
| Int _ -> () 
| Float _ -> ()
| Var id when (String.starts_with ~prefix:"tmp" id) -> let length_id = String.length id in 
                                                        let number =int_of_string_opt (get_int_string id 3 length_id) in 
                                                        (match number with
                                                        | None -> ()
                                                        | Some s -> remember_var:= IntSet.add s !remember_var)
                                                        
| Var _ -> ()
| Not t -> get_tmp_var_rec t
| Neg t -> get_tmp_var_rec t
| Add (l,r) -> get_tmp_var_rec l;get_tmp_var_rec r
| Sub (l , r) -> get_tmp_var_rec l ; get_tmp_var_rec r
| FNeg t -> get_tmp_var_rec t
| FAdd (l , r) -> get_tmp_var_rec l;get_tmp_var_rec r
| FSub (l,r) -> get_tmp_var_rec l;get_tmp_var_rec r
| FMul (l , r) -> get_tmp_var_rec l;get_tmp_var_rec r
| FDiv (l , r) -> get_tmp_var_rec l;get_tmp_var_rec r
| Eq (l , r) -> get_tmp_var_rec l ; get_tmp_var_rec r
| LE (l , r) -> get_tmp_var_rec l ; get_tmp_var_rec r
| Let ( v , before_in , after_in) -> get_tmp_var v;get_tmp_var_rec before_in; get_tmp_var_rec after_in
| If (c , then_ , else_ ) -> get_tmp_var_rec c;get_tmp_var_rec then_;get_tmp_var_rec else_
| LetRec (fundef , t) -> let name = fundef.name in
                          let argument = fundef.args in
                          let body = fundef.body in
                          get_tmp_var name;
                          List.iter get_tmp_var argument;
                          get_tmp_var_rec body 
                          ;get_tmp_var_rec t
| App (e,l) -> get_tmp_var_rec e;List.iter get_tmp_var_rec l
| Tuple t_list  -> List.iter get_tmp_var_rec t_list
| LetTuple ( l , before_in , after_in) ->List.iter get_tmp_var l;get_tmp_var_rec before_in;get_tmp_var_rec after_in 
| Array (l , r) ->   get_tmp_var_rec l;get_tmp_var_rec r
| Get (l , r) -> get_tmp_var_rec l;get_tmp_var_rec r
| Put (t1 , t2 , t3) -> get_tmp_var_rec t1;get_tmp_var_rec t2;get_tmp_var_rec t3


let normalization_function (syntax_t:Syntax.t) = value := 0;
  get_tmp_var_rec syntax_t;
  let int_list = IntSet.to_list (!remember_var) in
  print_string "[";
  List.iter (fun i -> print_int i ; print_char ',' ) int_list;
  print_endline "]";
  normalization_function_rec syntax_t
