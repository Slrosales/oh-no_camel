module M = Map.Make(String)

type t = string
let to_string x = x

let counter = ref 0

let genid () =
  incr counter;
  Printf.sprintf "?v%d" !counter

let genid_ s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter

let gentmp (_typ : Type.t) : t = 
  incr counter;
  Printf.sprintf "v%d" !counter