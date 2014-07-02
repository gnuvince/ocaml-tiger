%token <Src_pos.t * string> T_error
%token T_eof

%token T_kw_array
%token T_kw_do
%token T_kw_else
%token T_kw_end
%token T_kw_for
%token T_kw_function
%token T_kw_if
%token T_kw_in
%token T_kw_int
%token T_kw_let
%token T_kw_nil
%token T_kw_of
%token T_kw_string
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



%start <unit> program

%%

program:
| T_eof { () }
