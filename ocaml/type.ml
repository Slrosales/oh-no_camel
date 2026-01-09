type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var (ref None)

let rec to_string t =
  match t with
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | Fun (args, ret) ->
      let args_str = String.concat " -> " (List.map to_string args) in
      "(" ^ args_str ^ " -> " ^ to_string ret ^ ")"
  | Tuple elems -> "(" ^ String.concat " * " (List.map to_string elems) ^ ")"
  | Array t -> "[" ^ to_string t ^ "]"
  | Var { contents = None } -> "?" (* Unknown type variable *)
  | Var { contents = Some t } -> to_string t (* Follow the pointer *)
