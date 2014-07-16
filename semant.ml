open Ast
open Printf

type venv = Enventry.t Symtable.t
type tenv = Types.t Symtable.t

type expty = { expr: Translate.expr; typ: Types.t }


(* Ensure that a type is congruent to an expected type. *)
let assert_type expected actual =
  if not (Types.check expected actual) then
    failwith (sprintf "%s expected, %s found"
                (Types.to_string expected)
                (Types.to_string actual))



let rec trans_expr venv tenv (expr, pos) =
  match expr with
  | VarExpr v ->
     trans_var venv tenv v

  | NilExpr ->
     { typ=Types.Nil; expr=() }

  | IntExpr n ->
     { typ=Types.Int; expr=() }

  | StringExpr s ->
     { typ=Types.String; expr=() }

  | CallExpr call_rec ->
     let typ = check_call venv tenv call_rec in
     { typ=typ; expr=() }

  | OpExpr { op_left; op_right; op_op } ->
     let left = trans_expr venv tenv op_left in
     let right = trans_expr venv tenv op_right in
     let typ = check_op venv tenv left right op_op in
     { typ=typ; expr=() };

  | RecordExpr record_rec ->
     let typ = check_record venv tenv record_rec in
     { typ=typ; expr=() }

  | SeqExpr exprs ->
     List.fold_left
       (fun _ expr -> trans_expr venv tenv expr)
       {typ=Types.Unit; expr=()}
       exprs

  | IfExpr if_rec ->
     let typ = check_if venv tenv if_rec in
     { typ=typ; expr=() }

  | WhileExpr while_rec ->
     begin
       check_while venv tenv while_rec;
       { typ=Types.Unit; expr=() }
     end

  | ForExpr for_rec ->
     begin
       check_for venv tenv for_rec;
       { typ=Types.Unit; expr=() }
     end

  | BreakExpr ->
     { typ=Types.Unit; expr=() }

  | LetExpr let_rec ->
     check_let venv tenv let_rec

  | AssignExpr assign_rec ->
     check_assign venv tenv assign_rec

  | ArrayExpr array_rec ->
     check_array_expr venv tenv array_rec

and trans_var venv tenv var =
  match var with
  | SimpleVar (name, _) ->
     (match Symtable.find name venv with
     | Some (Enventry.VarEntry typ) -> { typ=typ; expr=() }
     | Some (Enventry.FunEntry _) -> failwith "syntax error"
     | None -> failwith (sprintf "unbound variable: %s" (Sym.to_string name))
     )



(* Convert an Ast.typ to Types.t, using a given type symbol table.
 * - For Ast.NameType, we simply lookup the name in the symbol table.
 * - For Ast.RecordType, we create a new Types.Record by looking up
 *   the type of every field.
 * - For Ast.ArrayType, we lookup the element type in the symbol table
 *   and create a Types.Array.
 *)
and trans_type tenv ast_type =
  match ast_type with
  | Ast.NameType (sym, _) ->
     find_type tenv sym

  | Ast.RecordType fields ->
     let rec_field_list =
       List.map
         (fun { field_name; field_type; _ } -> (field_name, find_type tenv field_type))
         fields
     in
     Types.Record (rec_field_list, ref ())

  | Ast.ArrayType (sym, _) ->
     let ty = find_type tenv sym in
     Types.Array (ty, ref ())

and find_type tenv ast_type =
  match Symtable.find ast_type tenv with
  | None -> failwith (sprintf "type is not in scope: %s" (Sym.to_string ast_type))
  | Some t -> t

and trans_field tenv { field_type; _ } =
  find_type tenv field_type

(* Dispatch the translation of a variable, function or type declaration. *)
and trans_decl venv tenv decl =
  match decl with
  | VarDecl var_rec   -> trans_var_decl venv tenv var_rec
  | FunDecl funs      -> trans_fun_decls venv tenv funs
  | TypeDecl types    -> trans_type_decls venv tenv types


(* If a var declaration has no explicit type declaration, we give it
 * the type of its initialization expression.  If a variable has an
 * explicit type declaration, we check it against the inferred type of
 * the initialization expression.  If they match, we give the var the
 * specified type; if they don't, we throw an exception.  If the
 * specified type does not exist, we throw an exception.
 *
 * nil is treated specially: it can only be assigned to a record
 * variable, and only if there is an explicit type declaration.
 *)
and trans_var_decl venv tenv { var_name; var_type; var_expr; _ } =
  let { typ=expr_type; _ } = trans_expr venv tenv var_expr in
  let venv' =
    match (expr_type, var_type) with
    | (Types.Nil, None) ->
       failwith "can only assign nil to type-declared variables"

    | (Types.Nil, Some type_sym) ->
       (match check_nil tenv type_sym with
       | None -> failwith "can only assign nil to record variables"
       | Some typ -> Symtable.add var_name (Enventry.VarEntry typ) venv
       )

    | (_, None) ->
       Symtable.add var_name (Enventry.VarEntry expr_type) venv

    | (_, Some type_sym) ->
       (match Symtable.find type_sym tenv with
       | None -> failwith (sprintf "type not in scope: %s" (Sym.to_string type_sym))
       | Some typ ->
          assert_type typ expr_type;
          Symtable.add var_name (Enventry.VarEntry typ) venv)
  in
  (venv', tenv)


and check_nil tenv type_sym =
  match Symtable.find type_sym tenv with
  | None -> failwith (sprintf "type not in scope: %s" (Sym.to_string type_sym))
  | Some t ->
     begin
       match Types.actual_type t with
       | Types.Record _ as t -> Some t
       | _ -> None
     end

and trans_fun_decls venv tenv decls =
  (* Add all the function definitions into a new variable symbol table
   * so that the functions can be mutually recursive.
   *)
  let new_venv =
    List.fold_left
      (fun curr_venv { fun_name; fun_params; fun_type; _ } ->
        let formal_types = List.map (trans_field tenv) fun_params in
        let return_type =
          match fun_type with
          | None -> Types.Unit
          | Some sym -> find_type tenv sym
        in
        Symtable.add
          fun_name
          (Enventry.FunEntry { fun_formals=formal_types; fun_result=return_type })
          curr_venv
      )
      venv
      decls
  in
  List.iter
    (fun { fun_params; fun_body; _ } ->
      let new_venv' = add_fields new_venv tenv fun_params in
      ignore (trans_expr new_venv' tenv fun_body)
    )
    decls;
  (new_venv, tenv)



(* Type declarations are contained in a list, and within that list, these
 * types may be mutually recursive.  We start by adding all the type names
 * to the type symbol table with a ref to None for their actual type.  After,
 * we update that ref with the actual type.
 *
 * TODO: detect cycles.
 *)
and trans_type_decls venv tenv decls =
  (* Add all the type names to the type symtable. *)
  let tenv' =
    List.fold_left
      (fun curr_tenv { type_name; _ } ->
        Symtable.add type_name (Types.Name (type_name, ref None)) curr_tenv)
      tenv
      decls
  in

  (* Update the ref field for Types.Name *)
  List.iter (fun { type_name; type_type; _ } ->
    match Symtable.find type_name tenv' with
    | Some (Types.Name (sym, ref_)) ->
       begin
         match !ref_ with
         | None -> ref_ := Some (trans_type tenv' type_type)
         | Some t -> failwith (sprintf "type %s already has a definition: %s"
                                 (Sym.to_string sym) (Types.to_string t))
       end
    | Some _
    | None -> failwith (sprintf "type is not in scope: %s" (Sym.to_string type_name))
  ) decls;
  (venv, tenv')


(* Type check a function call.  The name of the function (which must be a symbol)
 * is lookup in the symbol table; if it isn't bound or bound to a variable entry,
 * an exception is raised.  If the name refers to a function, we type check the
 * actual argument types to the expected argument types.  If they all match, the
 * type of the result is returned, otherwise an exception is raised.
 *)
and check_call venv tenv { call_func; call_args } =
  match Symtable.find call_func venv with
  | None ->
     failwith (sprintf "function not found: %s" (Sym.to_string call_func))

  | Some (Enventry.VarEntry _) ->
     failwith (sprintf "not a function identifier: %s" (Sym.to_string call_func))

  | Some (Enventry.FunEntry { Enventry.fun_formals; Enventry.fun_result }) ->
     let arg_types = List.map (fun expr -> (trans_expr venv tenv expr).typ) call_args in
     List.iter2 (fun expected actual ->
       if not (Types.check expected actual) then
         failwith (sprintf "type error in call to %s: %s expected, %s found"
                     (Sym.to_string call_func)
                     (Types.to_string expected)
                     (Types.to_string actual))
     ) fun_formals arg_types;
     fun_result

(* Type check a binary operation expression. In the case where the operator
 * is +, -, *, /, or %, both operands must be of type int.  For comparison
 * operators, the operands may be ints or strings: we check that both operands
 * are of the same type.  The result of a binary operation expression is always
 * an int.
 *)
and check_op venv tenv {typ=typ_left; _} {typ=typ_right; _} op_op =
  (match op_op with
  | OpPlus | OpMinus | OpTimes | OpDivide | OpModulo ->
     assert_type Types.Int typ_left;
     assert_type Types.Int typ_right;

  | OpEq | OpNeq | OpLt | OpLe | OpGt | OpGe ->
     begin
       match typ_left with
       | Types.Int -> assert_type Types.Int typ_right
       | Types.String -> assert_type Types.String typ_right
       | _ -> failwith "binary operations only work on ints and strings"
     end
  );
  Types.Int


(* Type check a record initialization expression.  If the name of the type
 * is not bound or does not refer to a record type, we throw an exception.
 * Otherwise, we make sure that the actual field types match the formal
 * field types.  If they don't we throw an exception, otherwise we return
 * the record type.
 *)
and check_record venv tenv { record_type; record_fields } =
  match Symtable.find record_type tenv with
  | None -> failwith (sprintf "type %s does not exist" (Sym.to_string record_type))
  | Some typ ->
     begin
       match Types.actual_type typ with
       | Types.Record (fields, un) as r ->
          let formals = List.map snd fields in
          let actuals = record_expr_types venv tenv record_fields in
          if List.for_all2 (fun form act -> Types.check form act) formals actuals then
            r
          else
            failwith "some expressions are ill-typed"
       | _ -> failwith (sprintf "type %s is not a record type" (Types.to_string typ))
     end

(* Extract the types of the field expressions of a record initialization.  *)
and record_expr_types venv tenv fields =
  List.map (fun (_, expr, _) ->
    let { typ; _ } = trans_expr venv tenv expr in typ)
    fields


(* Check an if-then or if-then-else expression.
 * - if-then: make sure the condition is an int and the then branch
 *   is a unit.
 *
 * - if-then-else: make sure the condition is an int and that the types
 *   of the then and else branches are the same.  If they're not, throw
 *   an exception.
 *)
and check_if venv tenv { if_test; if_then; if_else } =
  match if_else with
  | None ->
     begin
       let { typ=typ_test; _ } = trans_expr venv tenv if_test in
       let { typ=typ_then; _ } = trans_expr venv tenv if_then in
       assert_type Types.Int typ_test;
       assert_type Types.Unit typ_then;
       Types.Unit
     end
  | Some else_expr ->
     begin
       let { typ=typ_test; _ } = trans_expr venv tenv if_test in
       let { typ=typ_then; _ } = trans_expr venv tenv if_then in
       let { typ=typ_else; _ } = trans_expr venv tenv else_expr in
       assert_type Types.Int typ_test;
       if Types.check typ_then typ_else then
         typ_then
       else
         failwith (sprintf "then and else clause have different types: %s and %s"
                     (Types.to_string typ_then) (Types.to_string typ_else))
     end

(* Check a while expression; the loop test should be of type int and
 * the body should have type unit.
 *)
and check_while venv tenv { while_test; while_body } =
  let { typ=test_type; _ } = trans_expr venv tenv while_test in
  let { typ=body_type; _ } = trans_expr venv tenv while_body in
  assert_type Types.Int test_type;
  assert_type Types.Unit body_type

(* Check a for expression; the loop's lower and upper bounds should be
 * ints and the body should have type unit.
 *)
and check_for venv tenv { for_var; for_lo; for_hi; for_body; _ } =
  let { typ=lo_type; _ } = trans_expr venv tenv for_lo in
  let { typ=hi_type; _ } = trans_expr venv tenv for_hi in
  let { typ=body_type; _ } = trans_expr venv tenv for_body in
  assert_type Types.Int lo_type;
  assert_type Types.Int hi_type;
  assert_type Types.Unit body_type


(* Check a let expression: update the variable and type symbol
 * tables with the new decalarations and then translate the
 * body.
 *)
and check_let venv tenv { let_decls; let_body } =
  (* Process all the declarations to create updated versions of venv and tenv. *)
  let (new_venv, new_tenv) =
    List.fold_left
      (fun (curr_venv, curr_tenv) decl ->
        trans_decl curr_venv curr_tenv decl)
      (venv, tenv)
      let_decls
  in
  trans_expr new_venv new_tenv let_body

and check_assign venv tenv { assign_lhs; assign_rhs } =
  let { typ=lhs_type; _ } = trans_var venv tenv assign_lhs in
  let { typ=rhs_type; _ } = trans_expr venv tenv assign_rhs in
  assert_type lhs_type rhs_type;
  { typ=Types.Unit; expr=() }

and check_array_expr venv tenv { array_type; array_size; array_init } =
  let arr_type =
    match Symtable.find array_type tenv with
    | None -> failwith (sprintf "type not in scope: %s" (Sym.to_string array_type))
    | Some ty -> ty
  in
  let { typ=size_type; _ } = trans_expr venv tenv array_size in
  let { typ=init_type; _ } = trans_expr venv tenv array_init in

  assert_type Types.Int size_type;
  match Types.actual_type arr_type with
  | Types.Array (act_typ, _) ->
     assert_type act_typ init_type;
    { typ=arr_type; expr=() }
  | t ->
     failwith (sprintf "not an array type: %s" (Types.to_string t))

and add_field venv tenv { field_name; field_type; _ } =
  Symtable.add
    field_name
    (Enventry.VarEntry (find_type tenv field_type))
    venv

and add_fields venv tenv fields =
  List.fold_left
    (fun curr_venv field -> add_field curr_venv tenv field)
    venv
    fields
