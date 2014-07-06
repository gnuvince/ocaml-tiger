%{

open Ast

type var_tail =
  | TailSubscript of expr
  | TailField of sym

let rec make_var var pos lst =
  match lst with
  | [] -> var
  | h::t ->
     begin
       match h with
       | TailSubscript expr -> make_var (SubscriptVar (var, expr, pos)) pos t
       | TailField sym -> make_var (FieldVar (var, sym, pos)) pos t
     end

let make_src_pos () =
  let start = Parsing.symbol_start_pos ()
  and stop = Parsing.symbol_end_pos () in
  Src_pos.create
    start.Lexing.pos_fname
    start.Lexing.pos_lnum
    (start.Lexing.pos_cnum - start.Lexing.pos_bol)
    stop.Lexing.pos_lnum
    (stop.Lexing.pos_cnum - stop.Lexing.pos_bol)

%}


%token T_eof

%token T_kw_array
%token T_kw_break
%token T_kw_do
%token T_kw_else
%token T_kw_end
%token T_kw_for
%token T_kw_function
%token T_kw_if
%token T_kw_in
%token T_kw_let
%token T_kw_nil
%token T_kw_of
%token T_kw_then
%token T_kw_to
%token T_kw_type
%token T_kw_var
%token T_kw_while

%token T_sym_plus
%token T_sym_times
%token T_sym_minus
%token T_sym_divide
%token T_sym_modulo
%token T_sym_ampersand
%token T_sym_pipe
%token T_sym_colon
%token T_sym_semicolon
%token T_sym_colon_eq
%token T_sym_eq
%token T_sym_neq
%token T_sym_lbrace
%token T_sym_rbrace
%token T_sym_lbracket
%token T_sym_rbracket
%token T_sym_lparen
%token T_sym_rparen
%token T_sym_lt
%token T_sym_le
%token T_sym_ge
%token T_sym_gt
%token T_sym_dot
%token T_sym_comma

%token <int> T_lit_int
%token <string> T_lit_string
%token <string> T_ident

%nonassoc T_sym_colon_eq
%left T_sym_ampersand T_sym_pipe
%nonassoc T_sym_lt T_sym_le T_sym_gt T_sym_ge
%nonassoc T_sym_eq T_sym_neq
%left T_sym_plus T_sym_minus
%left T_sym_times T_sym_divide T_sym_modulo


%start <Ast.expr> program

%%

program:
| e=expr T_eof { e }

expr:
| v=var
    { VarExpr v }

| T_kw_nil
    { NilExpr }

| n=T_lit_int
    { IntExpr n }

| s=T_lit_string
    { StringExpr (s, make_src_pos ()) }

| func=T_ident T_sym_lparen args=separated_list(T_sym_comma, expr) T_sym_rparen
    { CallExpr { call_func=func; call_args=args; call_pos=make_src_pos () } }

| T_kw_break
    { BreakExpr (make_src_pos ()) }

| typ=T_ident T_sym_lbrace fields=separated_list(T_sym_comma, record_field) T_sym_rbrace
    { RecordExpr { record_type=typ; record_fields=fields; record_pos=make_src_pos () } }

| T_sym_lparen exprs=separated_list(T_sym_semicolon, expr) T_sym_rparen
    { SeqExpr exprs }

| v=var T_sym_colon_eq e=expr
    { AssignExpr { assign_lhs=v; assign_rhs=e; assign_pos=make_src_pos () } }

| T_kw_if e1=expr T_kw_then e2=expr T_kw_else e3=expr
    { IfExpr { if_test=e1; if_then=e2; if_else=Some e3; if_pos=make_src_pos () } }

| T_kw_if e1=expr T_kw_then e2=expr
    { IfExpr { if_test=e1; if_then=e2; if_else=None; if_pos=make_src_pos () } }

| T_kw_while e1=expr T_kw_do e2=expr
    { WhileExpr { while_test=e1; while_body=e2; while_pos=make_src_pos () } }

| T_kw_for var=T_ident T_sym_colon_eq lo=expr T_kw_to hi=expr T_kw_do body=expr
    { ForExpr { for_var=var;
                for_escape=ref true;
                for_lo=lo; for_hi=hi;
                for_body=body;
                for_pos=make_src_pos () }
    }

| typ=T_ident T_sym_lbracket size=expr T_sym_rbracket T_kw_of init=expr
    { ArrayExpr { array_type=typ; array_size=size; array_init=init; array_pos=make_src_pos () } }

| T_kw_let decls=nonempty_list(decl) T_kw_in body=separated_list(T_sym_semicolon, expr) T_kw_end
    { LetExpr { let_decls=decls; let_body=body; let_pos=make_src_pos () } }

(* Operators *)
| T_sym_minus e=expr              { OpExpr { op_left=IntExpr 0; op_right=e; op_op=OpMinus; op_pos=make_src_pos () } }
| e1=expr T_sym_plus      e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpPlus  ; op_pos=make_src_pos () } }
| e1=expr T_sym_minus     e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpMinus ; op_pos=make_src_pos () } }
| e1=expr T_sym_times     e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpTimes ; op_pos=make_src_pos () } }
| e1=expr T_sym_divide    e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpDivide; op_pos=make_src_pos () } }
| e1=expr T_sym_modulo    e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpDivide; op_pos=make_src_pos () } }
| e1=expr T_sym_eq        e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpEq    ; op_pos=make_src_pos () } }
| e1=expr T_sym_neq       e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpNeq   ; op_pos=make_src_pos () } }
| e1=expr T_sym_lt        e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpLt    ; op_pos=make_src_pos () } }
| e1=expr T_sym_le        e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpLe    ; op_pos=make_src_pos () } }
| e1=expr T_sym_gt        e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpGt    ; op_pos=make_src_pos () } }
| e1=expr T_sym_ge        e2=expr { OpExpr { op_left=e1; op_right=e2; op_op=OpGe    ; op_pos=make_src_pos () } }
| e1=expr T_sym_pipe      e2=expr { IfExpr { if_test=e1; if_then=IntExpr 1; if_else=Some e2; if_pos=make_src_pos () } }
| e1=expr T_sym_ampersand e2=expr { IfExpr { if_test=e1; if_then=e2; if_else=Some (IntExpr 0); if_pos=make_src_pos () } }

var:
| id=T_ident rest=var_tail
    { let pos = make_src_pos () in
      make_var (SimpleVar (id, pos)) pos rest }

var_tail:
| T_sym_dot id=T_ident rest=var_tail
    { TailField(id) :: rest }
| T_sym_lbracket expr=expr T_sym_rbracket rest=var_tail
    { TailSubscript(expr) :: rest }
| (* empty *)
    { [] }

record_field:
| k=T_ident T_sym_eq v=expr
    { (k, v, make_src_pos ()) }

decl:
| v=var_decl { VarDecl v }
| ts=nonempty_list(type_decl) { TypeDecl ts }
| fs=nonempty_list(fun_decl) { FunDecl fs }

var_decl:
| T_kw_var var=T_ident T_sym_colon typ=T_ident T_sym_colon_eq expr=expr
    { { var_name=var; var_type=Some typ; var_expr=expr; var_escape=ref true; var_pos=make_src_pos () } }
| T_kw_var var=T_ident T_sym_colon_eq expr=expr
    { { var_name=var; var_type=None; var_expr=expr; var_escape=ref true; var_pos=make_src_pos () } }

type_decl:
| T_kw_type name=T_ident T_sym_eq typ=typ
    { { type_name=name; type_type=typ; type_pos=make_src_pos () } }

fun_decl:
| T_kw_function name=T_ident T_sym_lparen params=separated_list(T_sym_comma, field) T_sym_rparen T_sym_colon typ=T_ident
    T_sym_eq body=expr
    { { fun_name=name; fun_params=params; fun_body=body; fun_type=Some typ; fun_pos=make_src_pos () } }
| T_kw_function name=T_ident T_sym_lparen params=separated_list(T_sym_comma, field) T_sym_rparen T_sym_eq body=expr
    { { fun_name=name; fun_params=params; fun_body=body; fun_type=None; fun_pos=make_src_pos () } }

typ:
| name=T_ident
    { NameType (name, make_src_pos ()) }
| T_sym_lbrace fields=separated_list(T_sym_comma, field) T_sym_rbrace
    { RecordType fields }
| T_kw_array T_kw_of name=T_ident
    { ArrayType (name, make_src_pos ()) }

field:
| name=T_ident T_sym_colon typ=T_ident
    { { field_name=name; field_type=typ; field_escape=ref true; field_pos=make_src_pos () } }
