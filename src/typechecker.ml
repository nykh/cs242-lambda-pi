open Core
open Ast.IR

exception TypeError of string

(* Checks that a type is legal. *)
let rec typecheck_type (tenv : String.Set.t) (tau : Type.t) : Type.t =
  match tau with
   | Type.Int -> tau
   | Type.Bool -> tau
   | Type.Fn (t1, t2) -> Type.Fn (typecheck_type tenv t1, typecheck_type tenv t2)

(* You need to implement the statics for the remaining cases below.
 * We have provided you with implementations of the other cases that you may
 * refer to.
 *
 * Note that you'll have to use OCaml sets to do this assignment. The semantics
 * for sets will be similar to maps. Like Maps, Sets are also immutable, so
 * any result from a set function will be different from the source. You may
 * find the functions Set.add and Set.find to be useful, in addition to others.
 *)
let rec typecheck_term (tenv : String.Set.t) (env : Type.t String.Map.t) (t : Term.t) : Type.t =
  match t with
  | Term.Int _ -> Type.Int
  | Term.Bool _ -> Type.Bool

  | Term.Var x ->
    (match Map.find env x with
     | Some tau -> tau
     | None -> raise (TypeError (Printf.sprintf "Unbound term variable %s" x)))

  | Term.Lam (x, arg_tau, body) ->
    let arg_tau' = typecheck_type tenv arg_tau in
    let env' = Map.add env ~key:x ~data:arg_tau' in
    let res_tau = typecheck_term tenv env' body in
    Type.Fn (arg_tau, res_tau)

  | Term.App (fn, arg) ->
    (match typecheck_term tenv env fn with
     | Type.Fn (arg_tau, res_tau) ->
       let arg_tau' = typecheck_term tenv env arg in
       if (Type.aequiv arg_tau arg_tau') then res_tau
       else raise (TypeError "Argument is of incorrect type")
     | _ -> raise (TypeError "Trying to apply non-Function"))

  | Term.Binop (_, t1, t2) ->
    let tau1 = typecheck_term tenv env t1 in
    let tau2 = typecheck_term tenv env t2 in
    (match (tau1, tau2) with
     | (Type.Int, Type.Int) -> Type.Int
     | _ -> raise (TypeError "Trying to binop on non-Int"))

  | Term.Logop (_, t1, t2) ->
    let tau1 = typecheck_term tenv env t1 in
    let tau2 = typecheck_term tenv env t2 in
    (match (tau1, tau2) with
     | (Type.Bool, Type.Bool) -> Type.Bool
     | _ -> raise (TypeError "Trying to logop on non-Bool"))

  | Term.Lognot t ->
    (match (typecheck_term tenv env t) with
     | Type.Bool -> Type.Bool
     | _ -> raise (TypeError "Trying to logop on non-Bool"))

  | Term.Comp (_, t1, t2) ->
    let tau1 = typecheck_term tenv env t1 in
    let tau2 = typecheck_term tenv env t2 in
    (match (tau1, tau2) with
     | (Type.Int, Type.Int) -> Type.Bool
     | _ -> raise (TypeError "Trying to compare non-Int"))

   | Term.IfThenElse (cond, tt, tf) ->
     let taucond = typecheck_term tenv env cond in
     let tau1 = typecheck_term tenv env tt in
     let tau2 = typecheck_term tenv env tf in
     (match taucond with
      | Type.Bool -> if Type.aequiv tau1 tau2 then tau1
                     else raise (TypeError "two branches are not of same type")
      | _ -> raise (TypeError "Condition is non-Bool"))


let typecheck t =
  try Ok (typecheck_term String.Set.empty String.Map.empty t)
  with TypeError s -> Error s

(* let inline_tests () =
  print_endline "Testing typechecker";

  (* Typechecks Pack and Unpack*)
  let exist =
    Type.Exists("Y", Type.Int)
  in
  let pack =
    Term.TPack(Type.Int, Term.Int 5, exist)
  in
  let unpack =
    Term.TUnpack("Y", "y", pack, Term.Var "y")
  in
  assert(typecheck unpack = Ok(Type.Int));

  (* Typecheck Inject *)
  let inj =
    Term.Inject(Term.Int 5, Ast.Left, Type.Sum(Type.Int, Type.Int))
  in
  assert (typecheck inj = Ok(Type.Sum(Type.Int, Type.Int)));

  (* Typechecks Tuple *)
  let tuple =
    Term.Tuple(((Int 3), (Int 4)))
  in
  assert (typecheck tuple = Ok(Type.Product(Type.Int, Type.Int)));

  (* Typechecks Case *)
  let inj =
    Term.Inject(Term.Int 5, Ast.Left, Type.Sum(Type.Int, Type.Product(Type.Int, Type.Int)))
  in
  let case1 = ("case1", Term.Int 8)
  in
  let case2 = ("case2", Term.Int 0)
  in
  let switch = Term.Case(inj, case1, case2)
  in
  assert (typecheck switch = Ok(Type.Int));

  (* Inline Tests from Assignment 3 *)
  let t1 = Term.Lam ("x", Type.Int, Term.Var "x") in
  assert (typecheck t1 = Ok(Type.Fn(Type.Int, Type.Int)));

  let t2 = Term.Lam ("x", Type.Int, Term.Var "y") in
  assert (Result.is_error (typecheck t2));

  let t3 = Term.App (t1, Term.Int 3) in
  assert (typecheck t3 = Ok(Type.Int));

  let t4 = Term.App (t3, Term.Int 3) in
  assert (Result.is_error (typecheck t4));

  let t5 = Term.Binop (Ast.Add, Term.Int 0, t1) in
  assert (Result.is_error (typecheck t5));

  print_endline "  all tests passed" *)

(* let () = inline_tests () *)
