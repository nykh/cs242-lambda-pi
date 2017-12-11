open Core

type binop = Add | Sub | Mul | Div
[@@deriving sexp_of, sexp, compare]

type logop = And | Or
[@@deriving sexp_of, sexp, compare]

type comp = Gt | Ge | Eq | Ne | Le | Lt
[@@deriving sexp_of, sexp, compare]

module Lang : sig
  module Type : sig
    type t =
      | Int
      | Bool
      | Var of string
      | Fn of t * t
    [@@deriving sexp_of, sexp, compare]

    val to_string : t -> string
  end

  module Term : sig
    type t =
      | Int of int
      | Bool of bool
      | Var of string
      | Lam of string * Type.t * t
      | App of t * t
      | Binop of binop * t * t
      | Logop of logop * t * t
      | Lognot of t
      | Comp of comp * t * t
      | IfThenElse of t * t * t
    [@@deriving sexp_of, sexp, compare]

    val to_string : t -> string
  end
end

module IR : sig
  module Type : sig
    type t =
      | Int
      | Bool
      | Var of string
      | Fn of t * t
    [@@deriving sexp_of, sexp, compare]

    val to_string : t -> string

    val aequiv : t -> t -> bool
  end

  module Term : sig
    type t =
      | Int of int
      | Bool of bool
      | Var of string
      | Lam of string * Type.t * t
      | App of t * t
      | Binop of binop * t * t
      | Logop of logop * t * t
      | Lognot of t
      | Comp of comp * t * t
      | IfThenElse of t * t * t
    [@@deriving sexp_of, sexp, compare]

    val to_string : t -> string

    val substitute : string -> t -> t -> t
  end
end
