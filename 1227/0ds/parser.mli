type token =
  | LPAREN
  | RPAREN
  | FUN
  | RIGHT
  | HANDLER
  | LBRACE
  | RETURN
  | COMMA
  | SEMI
  | RBRACE
  | WITH
  | HANDLE
  | NUMBER of (int)
  | VAR of (string)
  | OP of (string)
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.e
