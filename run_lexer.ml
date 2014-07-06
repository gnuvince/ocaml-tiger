open Lexing

let rec display_tokens lexbuf =
  try
    let tok = Lexer.lex_tiger lexbuf in
    if tok = Parser.T_eof then
      print_newline ()
    else begin
      print_string (Token.to_string tok);
      print_char ' ';
      display_tokens lexbuf
    end
  with (Lexer.LexerError (pos, reason)) ->
      Printf.eprintf "error: %s: %s\n" (Src_pos.to_string pos) reason


let () =
  let num_args = Array.length Sys.argv in
  if num_args = 1 then
    let lexbuf = Lexing.from_channel stdin in
    display_tokens lexbuf
  else
    for i = 1 to num_args - 1 do
      let in_ch = open_in Sys.argv.(i) in
      try
        let lexbuf = Lexing.from_channel in_ch in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = Sys.argv.(i) };
        display_tokens lexbuf;
        close_in in_ch
      with e ->
        close_in_noerr in_ch
    done
