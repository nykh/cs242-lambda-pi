open Core

exception DivideByZeroExn

type binop = Add | Sub | Mul | Div
[@@deriving sexp_of, sexp, compare]

type logop = And | Or
[@@deriving sexp_of, sexp, compare]

type comp = Gt | Ge | Eq | Ne | Le | Lt
[@@deriving sexp_of, sexp, compare]

type sym = string
[@@deriving sexp_of, sexp, compare]

type kind = Star | Box
[@@deriving sexp_of, sexp, compare]

module Lang = struct
  module Expr = struct
    type t =
      | Int
      | Bool
      | AInt of int
      | ABool of bool
      | Var of sym
      | Lam of sym * t * t
      | App of t * t
      | Pi of sym * t * t
      | Kind of kind
      | Binop of binop * t * t
      | Logop of logop * t * t
      | Lognot of t
      | Comp of comp * t * t
      | IfThenElse of t * t * t
      | Let of sym * t * t * t
    [@@deriving sexp_of, sexp, compare]

    let to_string t = Sexp.to_string_hum (sexp_of_t t)
  end
end

module IR = struct
  module Expr = struct
    type t =
      | Int
      | Bool
      | AInt of int
      | ABool of bool
      | Var of sym
      | Lam of sym * t * t
      | App of t * t
      | Pi of sym * t * t
      | Kind of kind
      | Binop of binop * t * t
      | Logop of logop * t * t
      | Lognot of t
      | Comp of comp * t * t
      | IfThenElse of t * t * t
    [@@deriving sexp_of, sexp, compare]

    let to_string t = Sexp.to_string_hum (sexp_of_t t)

    let rec freeVar t = match t with
     | Int | Bool | AInt _ | ABool _ -> String.Set.empty
     | Binop (_, t1, t2) -> String.Set.union (freeVar t1) (freeVar t2)
     | Logop (_, t1, t2) -> freeVar (Binop (Add, t1, t2))
     | Comp (_, t1, t2) -> freeVar (Binop (Add, t1, t2))
     | Lognot t -> freeVar t
     | IfThenElse (c, tt, tf) -> String.Set.union (freeVar c) (String.Set.union (freeVar tt) (freeVar tf))
     | Var x -> String.Set.singleton x
     | App (f, a) -> String.Set.union (freeVar f) (freeVar a)
     | Lam (x, t, e) -> String.Set.union (freeVar t) (String.Set.remove (freeVar e) x)
     | Pi (x, t, e) -> freeVar (Lam (x, t, e))
     | Kind _ -> String.Set.empty

    (* replace all *free* ocurrences of x with t' in t *)
    let rec substitute x t' t =
      let fvx = freeVar t' in
      let cloneSym x b =
        let vars = String.Set.union fvx (freeVar b) in
        let rec loop x = if String.Set.mem vars x then loop (x ^ "'") else x in
        loop x
      in
      let abstr sub con x' ty e =
        let ty' = sub ty in
        if x = x' then con x ty' e
        else
          if String.Set.mem fvx x' then
            let x'' = cloneSym x' e in
            let e' = substitute x' (Var x'') e in
            con x'' ty' (sub e')
          else
            con x' ty' (sub e)
      in
      let rec sub t = match t with
        | Int | Bool | AInt _ | ABool _ -> t
        | Binop (op, t1, t2) -> Binop (op, sub t1, sub t2)
        | Logop (op, t1, t2) -> Logop (op, sub t1, sub t2)
        | Comp (op, t1, t2) -> Comp (op, sub t1, sub t2)
        | Lognot t -> Lognot (sub t)
        | IfThenElse (c, tt, tf) -> IfThenElse (sub c, sub tt, sub tf)
        | Var i -> if x = i then t' else t
        | App (t1, t2) -> App (sub t1, sub t2)
        | Lam (i, ty, e) -> abstr sub (fun i ty e -> Lam (i, ty, e)) i ty e
        | Pi (i, k, e) -> abstr sub (fun i k e -> Pi (i, k, e)) i k e
        | Kind _ -> t
      in
        sub t

    let subVar (x: sym) (x': sym) (t: t) : t = substitute x (Var x') t

    let rec aequiv t1 t2 = match (t1, t2) with
      | (Int, Int) | (Bool, Bool) -> true
      | (AInt t, AInt t') -> t = t'
      | (ABool t, ABool t') -> t = t'
      | (Binop (op, t1, t2), Binop (op', t1', t2')) -> op = op' && (aequiv t1 t1') && (aequiv t2 t2')
      | (Logop (op, t1, t2), Logop (op', t1', t2')) -> op = op' && (aequiv t1 t1') && (aequiv t2 t2')
      | (Comp (op, t1, t2), Comp (op', t1', t2')) -> op = op' && (aequiv t1 t1') && (aequiv t2 t2')
      | (Lognot t, Lognot t') -> t = t'
      | (IfThenElse (c, tt, tf), IfThenElse (c', tt', tf')) -> (aequiv c c') && (aequiv tt tt') && (aequiv tf tf')
      | (Var x, Var x') -> x = x'
      | (Kind k, Kind k') -> k = k'
      | (App (f, a), App (f', a')) -> (aequiv f f') && (aequiv a a')
      | (Lam (x, ty, e), Lam (x', ty', e')) -> (ty = ty') && (aequiv e' (subVar x x' e))
      | (Pi (x, ty, e), Pi (x', ty', e')) -> (ty = ty') && (aequiv e' (subVar x x' e))
      | _ -> false

    let whnf t =
      let rec spine t ass = match t with
        | App (f, a) -> spine f (a::ass)
        | Lam (i, _, e) -> (match ass with
                              | [] -> t
                              | (x::xs) -> spine (substitute i x e) xs)
        | f -> let app = fun f a -> App (f, a) in
          List.fold_left ass ~init:f ~f:app
      in
      spine t []

    let rec nf_exn t =
      let rec spine t ass = match t with
        | Binop (op, t1, t2) ->
          let f = match op with
            | Add -> (+) | Sub -> (-) | Mul -> ( * ) | Div -> (/)
          in (match (op, nf_exn t1, nf_exn t2) with
           | (Div, _, AInt 0) -> raise DivideByZeroExn
           | (_, AInt a, AInt b) -> AInt (f a b)
           | (op, t1', t2') -> Binop (op, t1', t2'))
        | Logop (op, t1, t2) ->
          let f = match op with
            | And -> (&&) | Or -> (||)
          in (match (nf_exn t1, nf_exn t2) with
            | (ABool a, ABool b) -> ABool (f a b)
            | (t1', t2') -> Logop (op, t1', t2'))
        | Comp (op, t1, t2) ->
          let f = match op with
            | Gt -> (>) | Ge -> (>=) | Eq -> (=)
            | Ne -> (<>) | Le -> (<=) | Lt -> (<)
          in (match (nf_exn t1, nf_exn t2) with
            | (AInt a, AInt b) -> ABool (f a b)
            | (t1', t2') -> Comp (op, t1', t2'))
        | Lognot t -> (match nf_exn t with
          | ABool b -> ABool (not b)
          | t' -> Lognot t')
        | IfThenElse (c, tt, tf) ->
          (match nf_exn c with
            | ABool true -> nf_exn tt
            | ABool false -> nf_exn tf
            | c' -> IfThenElse (c', nf_exn tt, nf_exn tf))
        | App (f, a) -> spine f (a::ass)
        | Lam (i, ty, e) -> (match ass with
                            | [] -> Lam (i, nf_exn ty, nf_exn e)
                            | (x::xs) -> spine (substitute i x e) xs)
        | Pi (i, k, e) ->
          let pi' = Pi (i, nf_exn k, nf_exn e) in
          let app = fun f a -> App (f, a) in
            List.fold_left (List.map ass ~f:nf_exn) ~init:pi' ~f:app
        | f -> let app = fun f a -> App (f, a) in
          List.fold_left (List.map ass ~f:nf_exn) ~init:f ~f:app
      in
        spine t []

    let nf t = try Ok (nf_exn t) with
      | DivideByZeroExn -> Error "divide by zero"

    let bequiv t1 t2 = match (nf t1, nf t2) with
      | Ok(a), Ok(b) -> aequiv a b
      | _ -> false

    let inline_tests () =
      assert (aequiv (Binop (Div, AInt 1, AInt 0)) (Binop (Div, AInt 1, AInt 0)));
      assert (not (bequiv (Binop (Div, AInt 1, AInt 0)) (Binop (Div, AInt 1, AInt 0))))

    let () = inline_tests ()
  end
end
