module Syntax = Core.Syntax

let normalization_function_rec (syntax_t:Syntax.t):Syntax.t = match syntax_t with
| Unit -> Unit
| Bool b -> Bool b
| Int i -> Int i
| Float f -> Float f
| Not t -> Not t (*to implement later*)
| Neg t -> Neg t (*to implement later*)
| Add (l,r) -> Add (l,r)
| Sub (l , r) -> Sub (l , r) 
| FNeg t -> FNeg t (*to implement later*)
| FAdd (l , r) -> FAdd (l , r)
| FSub (l,r) -> FSub (l,r)
| FMul (l , r) -> FMul (l , r) (*to implement later*)
| FDiv (l , r) -> FDiv (l , r) (*to implement later*)
| Eq (l , r) -> Eq (l , r) (*to implement later*)
| LE (l , r) -> LE (l , r) (*to implement later*)
| If (c , then_ , else_ ) -> If (c , then_ , else_ )
| Let ((id , type_ ) , left , right) -> Let ((id , type_ ) , left , right) (*to implement later*)
| Var id -> Var id (*to implement later*)
| LetRec (fundef , t) -> LetRec (fundef , t) (*to implement later*)
| App (l,r) -> App (l,r) (*to implement later*)
| Tuple t  -> Tuple t (*to implement later*)
| LetTuple ( l , t1 , t2) -> LetTuple (l , t1 , t2) (*to implement later*)
| Array (l , r) ->  Array (l , r) (*to implement later*)
| Get (l , r) -> Get (l , r) (*to implement later*)
| Put (t1 , t2 , t3) -> Put (t1 , t2 , t3) (*to implement later*)
let normalization_function (syntax_t:Syntax.t) = normalization_function_rec syntax_t
