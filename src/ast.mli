open Core

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

module Lang : sig
  module Expr : sig
    type t =
      | Int
      | Bool
      | AInt of int
      | ABool of bool
      | Vect of t * int
      | AVect of int list
      | VectAdd of t * t
      | VectCat of t * t
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
      | Fn of t * t
    [@@deriving sexp_of, sexp, compare]

    val to_string : t -> string
  end
end

module IR : sig
  module Expr : sig
    type t =
      | Int
      | Bool
      | AInt of int
      | ABool of bool
      | Vect of t * int
      | AVect of int list
      | VectAdd of t * t
      | VectCat of t * t
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

    val to_string : t -> string

    val substitute : sym -> t -> t -> t

    val aequiv : t -> t -> bool

    val bequiv : t -> t -> bool

    val whnf : t -> t
    val nf : t -> (t, string) Result.t
  end
end
