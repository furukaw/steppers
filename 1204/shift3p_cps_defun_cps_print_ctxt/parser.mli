type token =
  | LPAREN
  | RPAREN
  | FUN
  | RIGHT
  | SHIFT
  | RESET
  | VAR of (string)
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.e
