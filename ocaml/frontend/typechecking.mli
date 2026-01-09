(* frontend/typing.mli *)

exception Type_Error of Core.Syntax.t * Core.Type.t * Core.Type.t

(* The global environment of external functions *)
(* K-Normalization uses this to decide App vs ExtFunApp *)
val extenv : Type.t Id.M.t ref

(* The main entry point *)
val f : Core.Syntax.t -> Core.Syntax.t