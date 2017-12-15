open Core
open Ast

val typecheck : IR.Expr.t -> (IR.Expr.t, string) Result.t
