open Ast

let rec check_break_expr in_loop (expr, _) =
  match expr with
  | VarExpr _ -> true
  | NilExpr -> true
  | IntExpr _ -> true
  | StringExpr _ -> true
  | CallExpr { call_args; _ } ->
     List.for_all (check_break_expr in_loop) call_args
  | OpExpr { op_left; op_right; _ } ->
     check_break_expr in_loop op_left && check_break_expr in_loop op_right
  | RecordExpr { record_fields; _ } ->
      List.for_all (fun (_, init_expr, _) -> check_break_expr in_loop init_expr) record_fields
  | SeqExpr exprs ->
     List.for_all (check_break_expr in_loop) exprs
  | AssignExpr { assign_rhs; _ } -> check_break_expr in_loop assign_rhs
  | IfExpr { if_test; if_then; if_else } ->
      let if_else_ok =
        match if_else with
        | None -> true
        | Some if_else_expr -> check_break_expr in_loop if_else_expr
      in
      check_break_expr in_loop if_test &&
        check_break_expr in_loop if_then &&
        if_else_ok
  | WhileExpr { while_test; while_body } ->
     check_break_expr in_loop while_test &&
       check_break_expr true while_body
  | ForExpr { for_lo; for_hi; for_body; _ } ->
     check_break_expr in_loop for_lo &&
       check_break_expr in_loop for_hi &&
       check_break_expr true for_body
  | BreakExpr -> in_loop
  | ArrayExpr { array_size; array_init; _ } ->
     check_break_expr in_loop array_size &&
       check_break_expr in_loop array_init
  | LetExpr { let_decls; let_body } ->
     List.for_all (check_break_decl in_loop) let_decls &&
       check_break_expr in_loop let_body

and check_break_decl in_loop = function
  | VarDecl { var_expr; _ } -> check_break_expr in_loop var_expr
  | FunDecl fun_decls ->
     List.for_all (fun { fun_body; _ } -> check_break_expr in_loop fun_body) fun_decls
  | TypeDecl _ -> true

let assert_break expr =
  if not (check_break_expr false expr) then
    failwith "break outside of a loop"
