open Ast

let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Lexer.lex_tiger lexbuf in
  ignore (Semant.trans_expr Env.base_venv Env.base_tenv ast)
