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



(* Convert an Ast.typ to Types.t, using a given type symbol table. *)
let rec ast_to_type tenv ast_type =
  let find_type ast_type =
    match Symtable.find ast_type tenv with
    | None -> failwith (sprintf "type is not in scope: %s" (Sym.to_string ast_type))
    | Some t -> t
  in

  match ast_type with
  | Ast.NameType (sym, _) ->
     find_type sym

  | Ast.RecordType fields ->
     let rec_field_list =
       List.map
         (fun { field_name; field_type; _ } -> (field_name, find_type field_type))
         fields
     in
     Types.Record (rec_field_list, ref ())
  | Ast.ArrayType (sym, _) ->
     let ty = find_type sym in
     Types.Array (ty, ref ())


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


and trans_type tenv typ =
  Types.Nil


and trans_decl venv tenv decl =
  match decl with
  | VarDecl var_rec   -> trans_var_decl venv tenv var_rec
  | FunDecl funs      -> trans_fun_decls venv tenv funs
  | TypeDecl types    -> trans_type_decls venv tenv types

(* If a var declaration has no explicit type declaration, we give it
 * the type of its initialization expression.  If a type has an
 * explicit declaration, we check it against the inferred type of the
 * initialization expression.  If they match, we give the var the
 * specified type; if they don't, we throw an exception.  If the
 * specified type does not exist, we throw an exception.
 *)
and trans_var_decl venv tenv { var_name; var_type; var_expr; _ } =
  let { typ=expr_typ; _ } = trans_expr venv tenv var_expr in
  let venv' =
    match var_type with
    | None -> Symtable.add var_name (Enventry.VarEntry expr_typ) venv
    | Some ty_sym ->
       begin
         match Symtable.find ty_sym tenv with
         | None -> failwith (sprintf "type not in scope: %s" (Sym.to_string ty_sym))
         | Some ty ->
            assert_type ty expr_typ;
            Symtable.add var_name (Enventry.VarEntry ty) venv
       end
  in
  (venv', tenv)

and trans_fun_decls venv tenv decls =
  (venv, tenv)

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
         | None -> ref_ := Some (ast_to_type tenv' type_type)
         | Some t -> failwith (sprintf "type %s already has a definition: %s"
                                 (Sym.to_string sym) (Types.to_string t))
       end
    | Some _
    | None -> failwith (sprintf "type is not in scope: %s" (Sym.to_string type_name))
  ) decls;
  (venv, tenv')

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

and check_op venv tenv {typ=typ_left; _} {typ=typ_right; _} op_op =
  (match op_op with
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  | OpModulo ->
     begin
       assert_type Types.Int typ_left;
       assert_type Types.Int typ_right;
     end
  | OpEq
  | OpNeq
  | OpLt
  | OpLe
  | OpGt
  | OpGe ->
     begin
       match typ_left with
       | Types.Int -> assert_type Types.Int typ_right
       | Types.String -> assert_type Types.String typ_right
       | _ -> failwith "binary operations only work on ints and strings"
     end
  );
  Types.Int

and check_record venv tenv { record_type; record_fields } =
  match Symtable.find record_type tenv with
  | None -> failwith (sprintf "type %s does not exist" (Sym.to_string record_type))
  | Some typ ->
     begin
       match Types.actual_type typ with
       | Types.Record (fields, un) as r ->
          begin
            let formals = List.map snd fields in
            let actuals = record_expr_types venv tenv record_fields in
            if List.for_all2 (fun form act -> Types.check form act) formals actuals then
              r
            else
              failwith "some expressions are ill-typed"
          end
       | _ -> failwith (sprintf "type %s is not a record type" (Types.to_string typ))
     end

and record_expr_types venv tenv fields =
  List.map (fun (_, expr, _) ->
    let { typ; _ } = trans_expr venv tenv expr in typ)
    fields


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

and check_while venv tenv { while_test; while_body } =
  let { typ=test_type; _ } = trans_expr venv tenv while_test in
  let { typ=body_type; _ } = trans_expr venv tenv while_body in
  assert_type Types.Int test_type;
  assert_type Types.Unit body_type

and check_for venv tenv { for_var; for_lo; for_hi; for_body; _ } =
  let { typ=lo_type; _ } = trans_expr venv tenv for_lo in
  let { typ=hi_type; _ } = trans_expr venv tenv for_hi in
  let { typ=body_type; _ } = trans_expr venv tenv for_body in
  assert_type Types.Int lo_type;
  assert_type Types.Int hi_type;
  assert_type Types.Unit body_type


and check_let venv tenv { let_decls; let_body } =
  (* Process all the declarations to create updated versions of venv and tenv. *)
  let (new_venv, new_tenv) =
    List.fold_left
      (fun (curr_venv, curr_tenv) decl ->
        trans_decl curr_venv curr_tenv decl)
      (venv, tenv)
      let_decls
  in
  (* Process the body of the let expression. Return the type of the last expression. *)
  List.fold_left
    (fun _ expr -> trans_expr new_venv new_tenv expr)
    { typ=Types.Unit; expr=() }
    let_body

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
