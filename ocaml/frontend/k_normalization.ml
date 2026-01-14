module M = Map.Make (String)
module Type = Core.Type
module Id = Core.Id
module Syntax = Core.Syntax

type t =
  | Unit
  | Int of int
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Div of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* Syntax.If + Syntax.Eq *)
  | IfLE of Id.t * Id.t * t * t (* Syntax.If + Syntax.LE *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t (* External Array - Ray Tracer benchmark*)
  | ExtFunApp of
      Id.t * Id.t list (* External Function Application - Library func*)

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

(* Helper to guess types *)
let rec type_of (e : t) : Type.t =
  match e with
  | Unit -> Type.Unit
  | Int _ -> Type.Int
  | Not _ | Neg _ | Add _ | Sub _ -> Type.Int
  | IfEq (_, _, e1, _) | IfLE (_, _, e1, _) -> type_of e1
  | Let (_, _, e2) -> type_of e2
  | Var _ -> Type.Int
  | _ -> Type.Unit

let rec insert_let (e : t) (k : Id.t -> t) : t =
  match e with
  | Var x -> k x
  | Let ((x, t), e1, e2) -> Let ((x, t), e1, insert_let e2 k)
  | _ ->
      let ty = type_of e in
      let x = Id.gentmp ty in
      Let ((x, ty), e, k x)

let rec f (e : Syntax.t) : t =
  match e with
  | Syntax.Unit -> Unit
  | Syntax.Bool b -> Int (if b then 1 else 0) (* Syntax.Bool -> KNormal.Int *)
<<<<<<< HEAD
  | Syntax.Int i -> let id = Id.gentmp Type.Int in
                   Let ((id,Type.Int), Int i, Var id)
                   
   
=======
  | Syntax.Int i ->
      let id = Id.gentmp Type.Int in
      Let ((id, Type.Int), Int i, Var id)
>>>>>>> 7e9e9cfc76ff5eb34f879dc01706a0dfc4816014
  | Syntax.Var x -> Var x
  | Syntax.Not e -> insert_let (f e) (fun x -> Not x)
  | Syntax.Neg e -> insert_let (f e) (fun x -> Neg x)
  | Syntax.Add (e1, e2) ->
      insert_let (f e1) (fun x -> insert_let (f e2) (fun y -> Add (x, y)))
  | Syntax.Sub (e1, e2) ->
      insert_let (f e1) (fun x -> insert_let (f e2) (fun y -> Sub (x, y)))
  (* 'if e1 then e2 else e3', we check what e1 is *)
  | Syntax.Mul (e1, e2) ->
      insert_let (f e1) (fun x -> insert_let (f e2) (fun y -> Mul (x, y)))
  | Syntax.Div (e1, e2) ->
      insert_let (f e1) (fun x -> insert_let (f e2) (fun y -> Div (x, y)))
  | Syntax.If (e1, e2, e3) -> (
      match e1 with
      | Syntax.Not e -> f (Syntax.If (e, e3, e2))
      | Syntax.Eq (e_left, e_right) ->
          insert_let (f e_left) (fun x ->
              insert_let (f e_right) (fun y -> IfEq (x, y, f e2, f e3)))
      | Syntax.LE (e_left, e_right) ->
          insert_let (f e_left) (fun x ->
              insert_let (f e_right) (fun y -> IfLE (x, y, f e2, f e3)))
      | _ ->
          (* if x then ...' (Implicit boolean check) *)
          (* Treated as 'if x = 0 (false) then else_block else then_block' *)
          insert_let (f e1) (fun x ->
              insert_let (Int 0) (fun zero ->
                  (* If x = 0 (False), go e3. Else e2. *)
                  IfEq (x, zero, f e3, f e2))))
  (* Rename variables following v0, v1...vn order*)
  | Syntax.Let ((x, t), e1, e2) ->
      let e1_definition = f e1 in
      let e2_body = f e2 in
      Let ((x, t), e1_definition, e2_body)
  | Syntax.LetRec
      (*let rec f x y = ...body... in ...rest...*)
      ({ Syntax.name = x, t; Syntax.args = list_args; Syntax.body = e1 }, e2) ->
      let e1_body = f e1 in
      (* rest of the program *)
      let e2_rest = f e2 in

      (* map fields from Syntax.fundef to KNormal.fundef *)
      LetRec ({ name = (x, t); args = list_args; body = e1_body }, e2_rest)
  | Syntax.App (e, args) -> (
      match e with
      | Syntax.Var func_name when not (M.mem func_name !Typechecking.extenv) ->
          let ext_func =
            [
              "create_array";
              "create_float_array";
              "print_int";
              "print_newline";
              "sin";
              "cos";
              "sqrt";
              "abs_float";
              "int_of_float";
              "float_of_int";
              "truncate";
            ]
          in
          (* Known External Function *)
          if List.mem func_name ext_func then
            let rec bind_args args k =
              match args with
              | [] -> k []
              | a :: rest ->
                  insert_let (f a) (fun x ->
                      bind_args rest (fun xs -> k (x :: xs)))
            in
            bind_args args (fun x_list -> ExtFunApp (func_name, x_list))
          (* User-Defined Function Var *)
            else
            insert_let (f e) (fun f_var ->
                let rec bind_args args k =
                  match args with
                  | [] -> k []
                  | a :: rest ->
                      insert_let (f a) (fun x ->
                          bind_args rest (fun xs -> k (x :: xs)))
                in
                bind_args args (fun x_list -> App (f_var, x_list)))
      (* Complex Function Expression *)
      | _ ->
          insert_let (f e) (fun f_var ->
              let rec bind_args args k =
                match args with
                | [] -> k []
                | a :: rest ->
                    insert_let (f a) (fun x ->
                        bind_args rest (fun xs -> k (x :: xs)))
              in
              bind_args args (fun x_list -> App (f_var, x_list))))
  | Syntax.Tuple es ->
      let rec bind_all args k =
        match args with
        | [] -> k []
        | e :: rest ->
            insert_let (f e) (fun x -> bind_all rest (fun xs -> k (x :: xs)))
      in
      bind_all es (fun xs -> Tuple xs)
  | Syntax.LetTuple (xts, e1, e2) ->
      insert_let (f e1) (fun y -> LetTuple (xts, y, f e2))
  | Syntax.Get (e1, e2) ->
      insert_let (f e1) (fun x -> insert_let (f e2) (fun y -> Get (x, y)))
  | Syntax.Put (e1, e2, e3) ->
      insert_let (f e1) (fun x ->
          insert_let (f e2) (fun y ->
              insert_let (f e3) (fun z -> Put (x, y, z))))
  | Syntax.Array (e1, e2) ->
      insert_let (f e1) (fun size_var ->
          insert_let (f e2) (fun init_var ->
              ExtFunApp ("create_array", [ size_var; init_var ])))
  | Syntax.Float _ | Syntax.FNeg _ | Syntax.FAdd _ | Syntax.FSub _
  | Syntax.FMul _ | Syntax.FDiv _ ->
      failwith "Float operations not supported."
  | Syntax.Eq _ | Syntax.LE _ ->
      failwith "Comparisons (Eq/LE) are only supported inside 'if' statements."

let rec to_string = function
  | Unit -> "()"
  | Int i -> string_of_int i
  | Not x -> Printf.sprintf "not %s" x
  | Neg x -> Printf.sprintf "-(%s)" x
  | Add (x, y) -> Printf.sprintf "%s + %s" x y
  | Sub (x, y) -> Printf.sprintf "%s - %s" x y
<<<<<<< HEAD
  | Mul (x,y) ->Printf.sprintf "%s * %s" x y
  | Div (x,y) ->Printf.sprintf "%s / %s" x y
=======
  | Mul (x, y) -> Printf.sprintf "%s * %s" x y
  | Div (x, y) -> Printf.sprintf "%s / %s" x y
>>>>>>> 7e9e9cfc76ff5eb34f879dc01706a0dfc4816014
  | Var x -> x
  | IfEq (x, y, e1, e2) ->
      Printf.sprintf "if %s = %s then\n  %s\nelse\n  %s" x y (to_string e1)
        (to_string e2)
  | IfLE (x, y, e1, e2) ->
      Printf.sprintf "if %s <= %s then\n  %s\nelse\n  %s" x y (to_string e1)
        (to_string e2)
  | Let ((x, _), e1, e2) ->
      Printf.sprintf "let %s = %s in\n%s" x (to_string e1) (to_string e2)
  | LetRec (fundef, e) ->
      Printf.sprintf "let rec %s(%s) =\n  %s\nin\n%s" (fst fundef.name)
        (String.concat ", " (List.map (fun (name, _) -> name) fundef.args))
        (to_string fundef.body) (to_string e)
  | App (f, args) -> Printf.sprintf "%s(%s)" f (String.concat ", " args)
  | Tuple vars -> Printf.sprintf "(%s)" (String.concat ", " vars)
  | LetTuple (vars, y, e) ->
      Printf.sprintf "let (%s) = %s in\n%s"
        (String.concat ", " (List.map fst vars))
        y (to_string e)
  | Get (x, y) -> Printf.sprintf "%s.(%s)" x y
  | Put (x, y, z) -> Printf.sprintf "%s.(%s) <- %s" x y z
  | ExtArray x -> Printf.sprintf "ExtArray(%s)" x
  | ExtFunApp (f, args) ->
      Printf.sprintf "ExtCall %s(%s)" f (String.concat " " args)
