open Core
open Ast

let rec translate (t : Lang.Expr.t) : IR.Expr.t =
  match t with
  | Lang.Expr.Int -> IR.Expr.Int
  | Lang.Expr.Bool -> IR.Expr.Bool
  | Lang.Expr.AInt n -> IR.Expr.AInt n
  | Lang.Expr.ABool b -> IR.Expr.ABool b
  | Lang.Expr.Kind k -> IR.Expr.Kind k
  | Lang.Expr.Var v -> IR.Expr.Var v
  | Lang.Expr.Lam (v, t, e) -> IR.Expr.Lam (v, translate t, translate e)
  | Lang.Expr.Pi (v, k, e) -> IR.Expr.Pi (v, translate k, translate e)
  | Lang.Expr.App (l, r) -> IR.Expr.App (translate l, translate r)
  | Lang.Expr.Binop (b, l, r) -> IR.Expr.Binop (b, translate l, translate r)
  | Lang.Expr.Logop (b, l, r) -> IR.Expr.Logop (b, translate l, translate r)
  | Lang.Expr.Lognot t -> IR.Expr.Lognot (translate t)
  | Lang.Expr.Comp (b, l, r) -> IR.Expr.Comp (b, translate l, translate r)
  | Lang.Expr.IfThenElse (cond, tt, tf) ->
      IR.Expr.IfThenElse (translate cond, translate tt, translate tf)
  | Lang.Expr.Let (x, t, a, e) ->
      match t with
        | Lang.Expr.Fn (t1, t2) ->
          (match a with Lang.Expr.Lam (v, _, _) -> translate (Lang.Expr.Let (x, Lang.Expr.Pi (v, t1, t2), a, e)))
        | _ -> IR.Expr.App (IR.Expr.Lam (x, translate t, translate e), translate a)

let assert_print (t : Lang.Expr.t) (t' : IR.Expr.t) =
  let cond = translate (t) = t' in
    assert (if not cond then begin
      Printf.printf "Expected IR: %s\n" (IR.Expr.to_string t');
      Printf.printf "Actual IR: %s\n" (IR.Expr.to_string (translate t))
    end;
    cond)

let inline_tests () =
  assert_print (Lang.Expr.AInt 3) (IR.Expr.AInt 3);
  assert_print (Lang.Expr.ABool true) (IR.Expr.ABool true);
  assert_print (Lang.Expr.Let ("x", Lang.Expr.Int, Lang.Expr.AInt 3,
                Lang.Expr.Binop (Ast.Add, Lang.Expr.Var "x", Lang.Expr.AInt 4)))
               (IR.Expr.App (IR.Expr.Lam ("x", IR.Expr.Int,
                IR.Expr.Binop (Ast.Add, IR.Expr.Var "x", IR.Expr.AInt 4)), IR.Expr.AInt 3))
  (* let t =
    Lang.Expr.Let (
      Lang.Pattern.Alias(
        Lang.Pattern.Tuple (
          Lang.Pattern.Var ("a", Lang.Expr.Int),
          Lang.Pattern.Var ("b", Lang.Expr.Int)),
        "c", Lang.Expr.Product(Lang.Expr.Int, Lang.Expr.Int)),
      Lang.Expr.Tuple (Lang.Expr.Int 1, Lang.Expr.Int 2),
      Lang.Expr.Binop (Add, Lang.Expr.Var "a", Lang.Expr.Var "b"))
  in
  let t' =
    IR.Expr.App (
      IR.Expr.Lam ("a", IR.Expr.Int,
        IR.Expr.App (
          IR.Expr.Lam ("b", IR.Expr.Int,
            IR.Expr.App (
              IR.Expr.Lam ("c", IR.Expr.Product (IR.Expr.Int, IR.Expr.Int),
                IR.Expr.Binop (Add, IR.Expr.Var "a", IR.Expr.Var "b")),
              IR.Expr.Tuple (IR.Expr.Int 1, IR.Expr.Int 2))),
          IR.Expr.Int 2)),
      IR.Expr.Int 1)
  in
  assert_print t t'; *)
(*
  let t =
    Lang.Expr.Let (
      Lang.Pattern.Var ("x", Lang.Expr.Int),
      Lang.Expr.Int 3,
      Lang.Expr.Var "x")
  in
  let t' =
    IR.Expr.App (
      IR.Expr.Lam ("x", IR.Expr.Int, IR.Expr.Var "x"),
      IR.Expr.Int 3)
  in
  assert_print t t'; *)

let () = inline_tests ()
