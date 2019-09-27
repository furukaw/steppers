{
(* 補助的な変数、関数、型などの定義 *)
open Parser
}

(* 正規表現の略記 *)
(* [...] の中は character '...' でなくてはならない *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule token = parse
| space+ { token lexbuf }       (* スペースは読み飛ばす *)
| "(*" [^ '\n']* "\n"           (* ( * から行末まではコメント *)
	 { token lexbuf }
| "("	 { LPAREN }
| ")"	 { RPAREN }
| "true" { TRUE }
| "false" { FALSE }
| "fun"  { FUN }
| "->"   { RIGHT }
| "handler" { HANDLER }
| "{"    { LBRACK }
| "}"    { RBRACK }
| ","    { COMMA }
| "return" { RETURN }
| ";"    { SEMI }
| "."    { DOT }
| "do"   { DO }
| "<-"   { LEFT }
| "in"   { IN }
| "if"   { IF }
| "then" { THEN }
| "else" { ELSE }
| "with" { WITH }
| "handle" { HANDLE }
| "\"" [^ '"']* "\""
	 { let s = Lexing.lexeme lexbuf in
	   STRING (String.sub s 1 (String.length s - 2)) }
| "read" { READ }
| "print" { PRINT }
| "raise" { RAISE }
| "decide" { DECIDE }
| "join" { JOIN }
| "+"    { PLUS }
| "-"    { MINUS }
| "max"  { MAX }
| digit+                        (* 数字が１個以上 *)
	 { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| (lower | '_')+(alpha | digit | '_')*
  	 { VAR (Lexing.lexeme lexbuf) }
| eof	 { EOF }                (* 入力終了 *)
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }
