open Ast

type venv = Enventry.t Symtable.t
type tenv = Types.t Symtable.t

type expty = { expr: Translate.expr; typ: Types.t }

let rec trans_expr venv tenv (expr, pos) =
  match expr with
  | VarExpr v -> trans_var venv tenv v
  | NilExpr -> { typ=Types.Nil; expr=() }

and trans_var venv tenv var =
  { typ=Types.Nil; expr=() }

and trans_decl venv tenv decl =
  (venv, tenv)

and trans_type tenv typ =
  Types.Nil
