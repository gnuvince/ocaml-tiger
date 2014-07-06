{
  open Parser

  exception LexerError of (Src_pos.t * string)

  let make_src_pos lexbuf =
    let start = Lexing.lexeme_start_p lexbuf
    and stop = Lexing.lexeme_end_p lexbuf in
    Src_pos.create
      start.Lexing.pos_fname
      start.Lexing.pos_lnum
      (start.Lexing.pos_cnum - start.Lexing.pos_bol)
      stop.Lexing.pos_lnum
      (stop.Lexing.pos_cnum - stop.Lexing.pos_bol)


  let keyword_tbl = Hashtbl.create 64
  let escape_tbl = Hashtbl.create 64
  let _ =
    List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
      [
        ("array"    , T_kw_array);
        ("break"    , T_kw_break);
        ("do"       , T_kw_do);
        ("else"     , T_kw_else);
        ("end"      , T_kw_end);
        ("for"      , T_kw_for);
        ("function" , T_kw_function);
        ("if"       , T_kw_if);
        ("in"       , T_kw_in);
        ("let"      , T_kw_let);
        ("nil"      , T_kw_nil);
        ("of"       , T_kw_of);
        ("then"     , T_kw_then);
        ("to"       , T_kw_to);
        ("type"     , T_kw_type);
        ("var"      , T_kw_var);
        ("while"    , T_kw_while)
      ];
    List.iter (fun (esc, tok) -> Hashtbl.add escape_tbl esc tok)
      [
        ("\\n" , '\n');
        ("\\r" , '\r');
        ("\\t" , '\t');
        ("\\"  , '\\');
        ("\""  , '\"')
      ]
}


let alpha = ['A'-'Z' 'a'-'z' '_']
let digit = ['0'-'9']
let alnum = alpha | digit
let ident = alpha alnum*
let hex_digit = digit | ['A'-'F' 'a'-'f']

rule lex_tiger = parse
| "/*" { lex_comment 0 lexbuf }

| '+'  { T_sym_plus }
| '*'  { T_sym_times }
| '-'  { T_sym_minus }
| '/'  { T_sym_divide }
| '%'  { T_sym_modulo }
| '&'  { T_sym_ampersand }
| '|'  { T_sym_pipe }
| ':'  { T_sym_colon }
| ';'  { T_sym_semicolon }
| ":=" { T_sym_colon_eq }
| '{'  { T_sym_lbrace }
| '}'  { T_sym_rbrace }
| '['  { T_sym_lbracket }
| ']'  { T_sym_rbracket }
| '('  { T_sym_lparen }
| ')'  { T_sym_rparen }
| '<'  { T_sym_lt }
| "<=" { T_sym_le }
| '='  { T_sym_eq }
| "<>" { T_sym_neq }
| ">=" { T_sym_ge }
| '>'  { T_sym_gt }
| '.'  { T_sym_dot }
| ','  { T_sym_comma }

| digit+ as num { T_lit_int (int_of_string num) }
| '"' { lex_string (Buffer.create 64) lexbuf }

| ident as id
    { try
        Hashtbl.find keyword_tbl id
      with Not_found ->
         T_ident id
    }
| [' ' '\t' '\n'] as sp
    { if sp = '\n' then Lexing.new_line lexbuf;
      lex_tiger lexbuf
    }
| eof { T_eof }
| _ as c
    { raise (LexerError (make_src_pos lexbuf,
                         Printf.sprintf "illegal character '%c'" c))
    }



and lex_string buf = parse
    | '"' { T_lit_string (Buffer.contents buf) }
    | ("\\n" | "\\r" | "\\t" | "\\\"" | "\\\\") as esc
        { let esc_chr = Hashtbl.find escape_tbl esc in
          Buffer.add_char buf esc_chr;
          lex_string buf lexbuf
        }
    | '\n'
        { raise (LexerError (make_src_pos lexbuf, "strings cannot contain newlines")) }
    | eof
        { raise (LexerError (make_src_pos lexbuf, "unfinished string")) }
    | _ as c
        { Buffer.add_char buf c;
          lex_string buf lexbuf
        }



and lex_comment depth = parse
    | "/*" { lex_comment (depth + 1) lexbuf }
    | "*/"
        { if depth = 0 then
            lex_tiger lexbuf
          else
            lex_comment (depth - 1) lexbuf
        }
    | eof
        { raise (LexerError (make_src_pos lexbuf, "unterminated comment")) }
    | _ { lex_comment depth lexbuf }



{
}
