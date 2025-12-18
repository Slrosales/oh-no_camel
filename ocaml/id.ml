type t = string

type l = L of string 

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    Printf.sprintf "v%d" !counter

let identifier (_typ : Type.t) : t =
  genid ()