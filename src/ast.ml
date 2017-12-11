open Core

type binop = Add | Sub | Mul | Div
[@@deriving sexp_of, sexp, compare]

type logop = And | Or
[@@deriving sexp_of, sexp, compare]

module Lang = struct
  module Type = struct
    type t =
      | Int
      | Bool
      | Var of string
      | Fn of t * t
    [@@deriving sexp_of, sexp, compare]

    let to_string t = Sexp.to_string_hum (sexp_of_t t)
  end

  module Term = struct
    type t =
      | Int of int
      | Bool of bool
      | Var of string
      | Lam of string * Type.t * t
      | App of t * t
      | Binop of binop * t * t
      | Logop of logop * t * t
      | Lognot of t
      | IfThenElse of t * t * t
    [@@deriving sexp_of, sexp, compare]

    let to_string t = Sexp.to_string_hum (sexp_of_t t)
  end
end


module IR = struct
  module Type = struct
    type t =
      | Int
      | Bool
      | Var of string
      | Fn of t * t
    [@@deriving sexp_of, sexp, compare]

    let to_string t = Sexp.to_string_hum (sexp_of_t t)

    let to_debruijn (tau : t) : t =
      let rec aux (depths : int String.Map.t) (tau : t) =
        let same_depth = aux depths in
        let incr_depth x tau =
          aux
            (String.Map.add
               (String.Map.map depths ~f:(fun x -> x + 1))
               ~key:x
               ~data:0)
          tau
        in
        match tau with
        | Int -> Int
        | Bool -> Bool
        | Var x ->
          Var (Int.to_string (
            match String.Map.find depths x with
            | Some n -> n
            | None -> 0))
        | Fn (tau1, tau2) -> Fn (same_depth tau1, same_depth tau2)
      in
      aux String.Map.empty tau

    let aequiv (tau1 : t) (tau2 : t) : bool =
      let rec aequiv_aux (tau1 : t) (tau2 : t) : bool =
        match (tau1, tau2) with
        | (Int, Int) -> true
        | (Bool, Bool) -> true
        | (Var x, Var x') -> x = x'
        | (Fn (arg1, ret1), Fn (arg2, ret2)) ->
          aequiv_aux arg1 arg2 && aequiv_aux ret1 ret2
        | _ -> false
      in
      aequiv_aux (to_debruijn tau1) (to_debruijn tau2)

    let inline_tests () =
      assert (aequiv Int Int);
      assert (aequiv Bool Bool);
      assert (aequiv (Fn (Int, Int)) (Fn (Int, Int)))

    let () = inline_tests ()
  end

  module Term = struct
    type t =
      | Int of int
      | Bool of bool
      | Var of string
      | Lam of string * Type.t * t
      | App of t * t
      | Binop of binop * t * t
      | Logop of logop * t * t
      | Lognot of t
      | IfThenElse of t * t * t
    [@@deriving sexp_of, sexp, compare]

    let to_string t = Sexp.to_string_hum (sexp_of_t t)

    let rec substitute x t' t =
      match t with
      | Int _ -> t
      | Bool _ -> t
      | Var x' -> if x = x' then t' else t
      | Lam (x', ty, body) -> if x = x' then t else Lam (x', ty, substitute x t' body)
      | App (t1, t2) -> App (substitute x t' t1, substitute x t' t2)
      | Binop (b, t1, t2) -> Binop (b, substitute x t' t1, substitute x t' t2)
      | Logop (b, t1, t2) -> Logop (b, substitute x t' t1, substitute x t' t2)
      | Lognot (t1) -> Lognot (substitute x t' t1)
      | IfThenElse (cond, tt, tf) ->
        IfThenElse (substitute x t' cond, substitute x t' tt, substitute x t' tf)
  end
end
