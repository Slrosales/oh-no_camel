module M = Map.Make(String)

type t = string

type l = L of string 

let to_string x = x

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    Printf.sprintf "v%d" !counter
<<<<<<< HEAD
=======

let gentmp (_typ : Type.t) : t =
  genid ()
>>>>>>> d035597ab8d2bd3a11b6f7dd312debbdc70888a2
