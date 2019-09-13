type token =
  | LPAREN
  | RPAREN
  | FUN
  | ARROW
  | PLUS
  | SHIFT
  | RESET
  | SHIFT0
  | CONTROL
  | CONTROL0
  | NUMBER of (int)
  | VAR of (string)
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.e_t
