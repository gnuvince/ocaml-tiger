{
  open Parser

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
        ("do"       , T_kw_do);
        ("else"     , T_kw_else);
        ("end"      , T_kw_end);
        ("for"      , T_kw_for);
        ("function" , T_kw_function);
        ("if"       , T_kw_if);
        ("in"       , T_kw_in);
        ("int"      , T_kw_int);
        ("let"      , T_kw_let);
        ("nil"      , T_kw_nil);
        ("of"       , T_kw_of);
        ("string"   , T_kw_string);
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
| "(*" { lex_comment 0 lexbuf }

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

and lex_string buf = parse
    | '"' { T_lit_string (Buffer.contents buf) }
    | '\n'
        { let tok = T_error (make_src_pos lexbuf, "strings cannot contain newlines") in
          Lexing.new_line lexbuf;
          tok
        }
    | eof
        { let tok = T_error (make_src_pos lexbuf, "unfinished string") in
          Lexing.new_line lexbuf;
          tok
        }
    | ("\\n" | "\\r" | "\\t" | "\\\"" | "\\\\") as esc
        { let esc_chr = Hashtbl.find escape_tbl esc in
          Buffer.add_char buf esc_chr;
          lex_string buf lexbuf
        }
    | _ as c
        { Buffer.add_char buf c;
          lex_string buf lexbuf
        }



and lex_comment depth = parse
| "(*" { lex_comment (depth + 1) lexbuf }
| "*)"
    { if depth = 0 then
        lex_tiger lexbuf
      else
        lex_comment (depth - 1) lexbuf
    }
| eof { T_error (make_src_pos lexbuf, "unterminated comment") }
| _* { lex_comment depth lexbuf }




{
}
