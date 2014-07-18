type venv = Enventry.t Symtable.t
type tenv = Types.t Symtable.t

type expty = { expr: Translate.expr; typ: Types.t }

val trans_expr : venv -> tenv -> Ast.expr -> expty
val trans_var  : venv -> tenv -> Ast.var  -> expty
val trans_decl : venv -> tenv -> Ast.decl -> (venv * tenv)
val trans_type :         tenv -> Ast.typ  -> Types.t
