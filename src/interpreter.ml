open Core
open Ast.IR

exception RuntimeError of string

type outcome =
  | Step of Term.t
  | Val
  | Err of string

(* You will implement the cases below. See the dynamics section
   for a specification on how the small step semantics should work. *)
let rec trystep (t : Term.t) : outcome =
  match t with
  | Term.Var _ -> raise (RuntimeError "Unreachable")

  | (Term.Lam _ | Term.Int _ | Term.Bool _) -> Val

  | Term.App (fn, arg) ->
    let sfn = trystep fn in
    (match sfn with
     | Step fn' -> Step (Term.App (fn', arg))
     | Err _ -> sfn
     | Val ->
       let sarg = trystep arg in
       (match sarg with
         | Step arg' -> Step (Term.App (fn, arg'))
         | Err _ -> sarg
         | Val -> (match fn with
           | Term.Lam (x, _, body) -> Step (Term.substitute x arg body)
           | _ -> Err "Trying to apply non-function")))

  | Term.Binop (b, t1, t2) ->
    let s1 = trystep t1 in
    (match s1 with
     | Step t1' -> Step (Term.Binop (b, t1', t2))
     | Err _ -> s1
     | Val ->
       let s2 = trystep t2 in
       (match s2 with
         | Step t2' -> Step (Term.Binop (b, t1, t2'))
         | Err _ -> s2
         | Val ->
           let op =  match b with
             | Ast.Add -> (+) | Ast.Sub -> (-) | Ast.Mul -> ( * ) | Ast.Div -> (/)
           in
           match (b, t1, t2) with
           | (Ast.Div, _, Term.Int 0) -> Err "Divide by zero"
           | (_, Term.Int a, Term.Int b) -> Step (Term.Int (op a b))
           | _ -> Err "Trying to perform binop on non-Integer"))

  | Term.Logop (b, t1, t2) ->
    let s1 = trystep t1 in
    (match s1 with
     | Step t1' -> Step (Term.Logop (b, t1', t2))
     | Err _ -> s1
     | Val ->
       let s2 = trystep t2 in
       (match s2 with
         | Step t2' -> Step (Term.Logop (b, t1, t2'))
         | Err _ -> s2
         | Val ->
           let op =  match b with
             | Ast.And -> (&&) | Ast.Or -> (||)
           in
           match (t1, t2) with
           | (Term.Bool a, Term.Bool b) -> Step (Term.Bool (op a b))
           | _ -> Err "Trying to perform logop on non-Bool"))

  | Term.Lognot t ->
    let s = trystep t in
    (match s with
      | Step t' -> Step (Term.Lognot t')
      | Err _ -> s
      | Val ->
        match t with
        | Term.Bool b -> Step (Term.Bool (not b))
        | _ -> Err "Trying to negate non-Bool")

  | Term.Comp (c, t1, t2) ->
    let s1 = trystep t1 in
    (match s1 with
     | Step t1' -> Step (Term.Comp (c, t1', t2))
     | Err _ -> s1
     | Val ->
       let s2 = trystep t2 in
       (match s2 with
         | Step t2' -> Step (Term.Comp (c, t1, t2'))
         | Err _ -> s2
         | Val ->
           let op =  match c with
             | Ast.Gt -> (>) | Ast.Ge -> (>=)
             | Ast.Eq -> (=) | Ast.Ne -> (<>)
             | Ast.Le -> (<=) | Ast.Lt -> (<)
           in
           match (t1, t2) with
           | (Term.Int a, Term.Int b) -> Step (Term.Bool (op a b))
           | _ -> Err "Trying to compare non-Integer"))

   | Term.IfThenElse (cond, tt, tf) ->
    let s = trystep cond in
    (match s with
      | Step cond' -> Step (Term.IfThenElse (cond', tt, tf))
      | Err _ -> s
      | Val ->
        match cond with
        | Term.Bool true -> Step tt
        | Term.Bool false -> Step tf
        | _ -> Err "Condition is non-Bool")

let rec eval e =
  match trystep e with
  | Step e' -> eval e'
  | Val -> Ok e
  | Err s -> Error s

let inline_tests () =
  (* Inline Tests from Assignment 3 *)
  let t1 = Term.Binop(Ast.Add, Term.Int 2, Term.Int 3) in
  assert (trystep t1 = Step(Term.Int 5));

  let t2 = Term.App(Term.Lam("x", Type.Int, Term.Var "x"), Term.Int 3) in
  assert (trystep t2 = Step(Term.Int 3));

  let t3 = Term.Binop(Ast.Div, Term.Int 3, Term.Int 0) in
  assert (match trystep t3 with Err _ -> true | _ -> false);

(* let () = inline_tests () *)
