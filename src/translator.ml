open Core
open Ast

let rec translate_type (t : Lang.Type.t) : IR.Type.t =
  match t with
  | Lang.Type.Int -> IR.Type.Int
  | Lang.Type.Bool -> IR.Type.Bool
  | Lang.Type.Var v -> IR.Type.Var v
  | Lang.Type.Fn (l, r) -> IR.Type.Fn (translate_type l, translate_type r)

(* translate_term converts a term from the top-level language (Lang.Term.t) into
 * a term in the intermediate representation (IR.Term.t). The conversion primarily
 * eliminates pattern matching. We have implemented the Match case for you, now it
 * is up to you to define the translation for Let. *)
let rec translate_term (t : Lang.Term.t) : IR.Term.t =
  match t with
  (* These translations are trivial (nothing changes), so we have implemented them
   * for you. *)
  | Lang.Term.Int n -> IR.Term.Int n
  | Lang.Term.Bool b -> IR.Term.Bool b
  | Lang.Term.Var v -> IR.Term.Var v
  | Lang.Term.Lam (v, t, e) -> IR.Term.Lam (v, translate_type t, translate_term e)
  | Lang.Term.App (l, r) -> IR.Term.App (translate_term l, translate_term r)
  | Lang.Term.Binop (b, l, r) -> IR.Term.Binop (b, translate_term l, translate_term r)
  | Lang.Term.Logop (b, l, r) -> IR.Term.Logop (b, translate_term l, translate_term r)
  | Lang.Term.Lognot t -> IR.Term.Lognot (translate_term t)
  | Lang.Term.Comp (b, l, r) -> IR.Term.Comp (b, translate_term l, translate_term r)
  | Lang.Term.IfThenElse (cond, tt, tf) ->
      IR.Term.IfThenElse (translate_term cond, translate_term tt, translate_term tf)

let translate t = translate_term t

let assert_print (t : Lang.Term.t) (t' : IR.Term.t) =
  let cond = translate_term (t) = t' in
    assert (if not cond then begin
      Printf.printf "t=%s\n" (IR.Term.to_string (translate_term t));
      Printf.printf "t'=%s\n" (IR.Term.to_string t')
    end;
    cond)

let inline_tests () =
  print_endline "Testing tranlator";
  assert_print (Lang.Term.Int 3) (IR.Term.Int 3);
  assert_print (Lang.Term.Bool true) (IR.Term.Bool true);

  (* let t =
    Lang.Term.Let (
      Lang.Pattern.Alias(
        Lang.Pattern.Tuple (
          Lang.Pattern.Var ("a", Lang.Type.Int),
          Lang.Pattern.Var ("b", Lang.Type.Int)),
        "c", Lang.Type.Product(Lang.Type.Int, Lang.Type.Int)),
      Lang.Term.Tuple (Lang.Term.Int 1, Lang.Term.Int 2),
      Lang.Term.Binop (Add, Lang.Term.Var "a", Lang.Term.Var "b"))
  in
  let t' =
    IR.Term.App (
      IR.Term.Lam ("a", IR.Type.Int,
        IR.Term.App (
          IR.Term.Lam ("b", IR.Type.Int,
            IR.Term.App (
              IR.Term.Lam ("c", IR.Type.Product (IR.Type.Int, IR.Type.Int),
                IR.Term.Binop (Add, IR.Term.Var "a", IR.Term.Var "b")),
              IR.Term.Tuple (IR.Term.Int 1, IR.Term.Int 2))),
          IR.Term.Int 2)),
      IR.Term.Int 1)
  in
  assert_print t t'; *)
(*
  let t =
    Lang.Term.Let (
      Lang.Pattern.Var ("x", Lang.Type.Int),
      Lang.Term.Int 3,
      Lang.Term.Var "x")
  in
  let t' =
    IR.Term.App (
      IR.Term.Lam ("x", IR.Type.Int, IR.Term.Var "x"),
      IR.Term.Int 3)
  in
  assert_print t t'; *)

  print_endline "  all tests passed"

(* let () = inline_tests () *)
