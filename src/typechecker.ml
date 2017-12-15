open Core
open Ast.IR

exception TypeError of string

let allowed _ _ = true

(* Checks that a type is legal. *)
let rec typecheck_expr (tenv : Expr.t String.Map.t) (e : Expr.t) : Expr.t =
  let typecheck_redex (tenv : Expr.t String.Map.t) (e : Expr.t) : Expr.t =
    let t = typecheck_expr tenv e in
      Expr.whnf t
  in
  match e with
  | Expr.Err -> e
  | Expr.AInt _ -> Expr.Int
  | Expr.ABool _ -> Expr.Bool
  | Expr.Int | Expr.Bool -> Expr.Kind Ast.Star
  | Expr.Kind Ast.Star -> Expr.Kind Ast.Box
  | Expr.Kind Ast.Box -> raise (TypeError "Encountered box")

  | Expr.Var x ->
    (match String.Map.find tenv x with
     | Some tau -> tau
     | None -> raise (TypeError (Printf.sprintf "Unbound term variable %s" x)))

  | Expr.Lam (x, t, body) ->
    let _ = typecheck_expr tenv t in
    let tenv' = String.Map.add tenv ~key:x ~data:t in
    let rt = typecheck_expr tenv' body in
    let pi = Expr.Pi (x, t, rt) in
    let _ = typecheck_expr tenv' pi in
    pi

  | Expr.Pi (x, k, body) ->
    let k' = typecheck_redex tenv k in
    let tenv' = String.Map.add tenv ~key:x ~data:k in
    let rt = typecheck_redex tenv' body in
    if (not (allowed k' rt)) then raise (TypeError "Wrong kinding")
    else rt

  | Expr.App (f, a) ->
    (match typecheck_redex tenv f with
     | Expr.Pi (x, at, rt) ->
       let at' = typecheck_expr tenv a in
       if (Expr.bequiv at at') then Expr.substitute x a rt
       else raise (TypeError "Argument is of incorrect type")
     | _ -> raise (TypeError "Trying to apply non-Function"))

  | Expr.Binop (_, t1, t2) ->
    let tau1 = typecheck_expr tenv t1 in
    let tau2 = typecheck_expr tenv t2 in
    (match (tau1, tau2) with
     | (Expr.Int, Expr.Int) -> Expr.Int
     | _ -> raise (TypeError "Trying to binop on non-Int"))

  | Expr.Logop (_, t1, t2) ->
    let tau1 = typecheck_expr tenv t1 in
    let tau2 = typecheck_expr tenv t2 in
    (match (tau1, tau2) with
     | (Expr.Bool, Expr.Bool) -> Expr.Bool
     | _ -> raise (TypeError "Trying to logop on non-Bool"))

  | Expr.Lognot t ->
    (match (typecheck_expr tenv t) with
     | Expr.Bool -> Expr.Bool
     | _ -> raise (TypeError "Trying to logop on non-Bool"))

  | Expr.Comp (_, t1, t2) ->
    let tau1 = typecheck_expr tenv t1 in
    let tau2 = typecheck_expr tenv t2 in
    (match (tau1, tau2) with
     | (Expr.Int, Expr.Int) -> Expr.Bool
     | _ -> raise (TypeError "Trying to compare non-Int"))

   | Expr.IfThenElse (cond, tt, tf) ->
     let taucond = typecheck_expr tenv cond in
     let tau1 = typecheck_expr tenv tt in
     let tau2 = typecheck_expr tenv tf in
     (match taucond with
      | Expr.Bool -> if Expr.aequiv tau1 tau2 then tau1
                     else raise (TypeError "two branches are not of same type")
      | _ -> raise (TypeError "Condition is non-Bool"))


let typecheck t =
  try Ok (typecheck_expr String.Map.empty t)
  with TypeError s -> Error s

let inline_tests () =
  let assertType e t =
    let rt = typecheck e in
    match rt with
      | Ok(rt') -> if rt' = t then ()
                  else print_endline (Printf.sprintf "Expected type: %s\nReal type: %s"
                                                     (Expr.to_string t) (Expr.to_string rt'))
      | Error s -> print_endline ("Type error occured: " ^ s)
  in

  (* Inline Tests from Assignment 3 *)
  let t1 = Expr.Lam ("x", Expr.Int, Expr.Var "x") in
  assert (typecheck t1 = Ok(Expr.Pi("x", Expr.Int, Expr.Int)));

  let t2 = Expr.Lam ("x", Expr.Int, Expr.Var "y") in
  assert (Result.is_error (typecheck t2));

  let t3 = Expr.App (t1, Expr.AInt 3) in
  assert (typecheck t3 = Ok(Expr.Int));

  let t4 = Expr.App (t3, Expr.AInt 3) in
  assert (Result.is_error (typecheck t4));

  let t5 = Expr.Binop (Ast.Add, Expr.AInt 0, t1) in
  assert (Result.is_error (typecheck t5));

  (* polymorphism *)
  let t6 = Expr.Lam ("X", Expr.Kind Ast.Star, Expr.Lam ("x", Expr.Var "X", Expr.Var "x")) in
  assertType t6 (Expr.Pi ("X", Expr.Kind Ast.Star, Expr.Pi ("x", Expr.Var "X", Expr.Var "X")));

  let t7 = Expr.App (t6, Expr.Int) in
  assertType t7 (Expr.Pi ("x", Expr.Int, Expr.Int))

let () = inline_tests ()
