type token =
  | LPAREN
  | RPAREN
  | FUN
  | RIGHT
  | HANDLER
  | LBRACK
  | RBRACK
  | RETURN
  | COMMA
  | SEMI
  | WITH
  | HANDLE
  | VAR of (string)
  | OP of (string)
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.e
