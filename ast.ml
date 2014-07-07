open Printf

type escape = bool ref

type decl =
  | VarDecl of var_decl
  | FunDecl of fun_decl list
  | TypeDecl of type_decl list

and expr = expr_ * Src_pos.t
and expr_ =
  | VarExpr of var
  | NilExpr
  | IntExpr of int
  | StringExpr of string
  | CallExpr of call_expr
  | OpExpr of op_expr
  | RecordExpr of record_expr
  | SeqExpr of expr list
  | AssignExpr of assign_expr
  | IfExpr of if_expr
  | WhileExpr of while_expr
  | ForExpr of for_expr
  | BreakExpr
  | LetExpr of let_expr
  | ArrayExpr of array_expr

and var =
  | SimpleVar of Sym.t * Src_pos.t
  | FieldVar of var * Sym.t * Src_pos.t
  | SubscriptVar of var * expr * Src_pos.t

and typ =
  | NameType of Sym.t * Src_pos.t
  | RecordType of field list
  | ArrayType of Sym.t * Src_pos.t

and field = { field_name: Sym.t;
              field_type: Sym.t;
              field_escape: escape;
              field_pos: Src_pos.t }

and var_decl = { var_name: Sym.t;
                 var_type: Sym.t option;
                 var_expr: expr;
                 var_escape: escape;
                 var_pos: Src_pos.t }

and fun_decl = { fun_name: Sym.t;
                 fun_params: field list;
                 fun_body: expr;
                 fun_type: Sym.t option;
                 fun_pos: Src_pos.t }

and type_decl = { type_name: Sym.t;
                  type_type: typ;
                  type_pos: Src_pos.t }

and op =
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  | OpModulo
  | OpEq
  | OpNeq
  | OpLt
  | OpLe
  | OpGt
  | OpGe

and call_expr = { call_func: Sym.t;
                  call_args: expr list }


and op_expr = { op_left: expr;
                op_right: expr;
                op_op: op }

and record_expr = { record_type: Sym.t;
                    record_fields: (Sym.t * expr * Src_pos.t) list }

and assign_expr = { assign_lhs: var;
                    assign_rhs: expr }

and if_expr = { if_test: expr;
                if_then: expr;
                if_else: expr option }

and while_expr = { while_test: expr;
                   while_body: expr }

and for_expr = { for_var: Sym.t;
                 for_escape: escape;
                 for_lo: expr;
                 for_hi: expr;
                 for_body: expr }

and let_expr = { let_decls: decl list;
                 let_body: expr list }

and array_expr = { array_type: Sym.t;
                   array_size: expr;
                   array_init: expr }



let rec expr_to_string (expr_, _) = expr__to_string expr_

and expr__to_string = function
  | VarExpr v ->
     var_to_string v

  | NilExpr ->
     "nil"

  | IntExpr n ->
     string_of_int n

  | StringExpr s ->
     sprintf "\"%s\"" (String.escaped s)

  | CallExpr { call_func; call_args } ->
     sprintf "%s(%s)"
       (Sym.to_string call_func)
       (String.concat ", " (List.map expr_to_string call_args))

  | OpExpr { op_left; op_right; op_op } ->
     sprintf "(%s %s %s)" (expr_to_string op_left) (op_to_string op_op) (expr_to_string op_right)

  | RecordExpr { record_type; record_fields } ->
     sprintf "%s{ %s }"
       (Sym.to_string record_type)
       (String.concat ", " (List.map (fun (k, v, _) -> sprintf "%s=%s" (Sym.to_string k) (expr_to_string v)) record_fields))

  | SeqExpr exprs ->
     sprintf "(%s)" (String.concat "; " (List.map expr_to_string exprs))

  | AssignExpr { assign_lhs; assign_rhs } ->
     sprintf "%s := %s" (var_to_string assign_lhs) (expr_to_string assign_rhs)

  | IfExpr { if_test; if_then; if_else } ->
     let else_str =
       (match if_else with
       | Some expr -> " else " ^ expr_to_string expr
       | None      -> ""
       ) in
     sprintf "if (%s) then %s%s" (expr_to_string if_test) (expr_to_string if_then) else_str

  | WhileExpr { while_test; while_body } ->
     sprintf "while %s do %s" (expr_to_string while_test) (expr_to_string while_body)

  | ForExpr { for_var; for_lo; for_hi; for_body; _ } ->
     sprintf "for %s := %s to %s do %s"
       (Sym.to_string for_var)
       (expr_to_string for_lo)
       (expr_to_string for_hi)
       (expr_to_string for_body)

  | BreakExpr ->
     "break"

  | LetExpr { let_decls; let_body } ->
     sprintf "let %s in %s end"
       (String.concat " " (List.map let_decl_to_string let_decls))
       (String.concat "; " (List.map expr_to_string let_body))

  | ArrayExpr { array_type; array_size; array_init } ->
     sprintf "%s [%s] of %s"
       (Sym.to_string array_type)
       (expr_to_string array_size)
       (expr_to_string array_init)


and var_to_string = function
  | SimpleVar (sym, _) -> Sym.to_string sym
  | FieldVar (var, field, _) -> sprintf "%s.%s" (var_to_string var) (Sym.to_string field)
  | SubscriptVar (var, expr, _) -> sprintf "%s[%s]" (var_to_string var) (expr_to_string expr)

and let_decl_to_string = function
  | VarDecl var_decl ->
     var_decl_to_string var_decl
  | FunDecl fun_decls ->
     String.concat " " (List.map fun_decl_to_string fun_decls)
  | TypeDecl type_decls ->
     String.concat " " (List.map type_decl_to_string type_decls)

and var_decl_to_string { var_name; var_type; var_expr; _ } =
  sprintf "var %s%s := %s"
    (Sym.to_string var_name)
    (match var_type with
    | Some t -> ": " ^ Sym.to_string t
    | None -> "")
    (expr_to_string var_expr)

and fun_decl_to_string { fun_name; fun_params; fun_body; fun_type; fun_pos=_ } =
  sprintf "function %s(%s)%s = %s"
    (Sym.to_string fun_name)
    (String.concat ", " (List.map field_to_string fun_params))
    (match fun_type with
    | Some t -> ": " ^ Sym.to_string t
    | None -> "")
    (expr_to_string fun_body)

and type_decl_to_string { type_name; type_type; type_pos=_ } =
  sprintf "type %s = %s"
    (Sym.to_string type_name)
    (type_to_string type_type)

and field_to_string { field_name; field_type; _ } =
  sprintf "%s: %s"
    (Sym.to_string field_name)
    (Sym.to_string field_type)

and type_to_string = function
  | NameType (name, _) -> Sym.to_string name
  | RecordType fields -> sprintf "{ %s }" (String.concat ", " (List.map field_to_string fields))
  | ArrayType (base_type, _) -> sprintf "array of %s" (Sym.to_string base_type)

and op_to_string = function
  | OpPlus   -> "+"
  | OpMinus  -> "-"
  | OpTimes  -> "*"
  | OpDivide -> "/"
  | OpModulo -> "%"
  | OpEq     -> "="
  | OpNeq    -> "<>"
  | OpLt     -> "<"
  | OpLe     -> "<="
  | OpGt     -> ">"
  | OpGe     -> ">="
