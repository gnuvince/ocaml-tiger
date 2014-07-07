exception LexerError of (Src_pos.t * string)

val lex_tiger : Lexing.lexbuf -> Parser.token
