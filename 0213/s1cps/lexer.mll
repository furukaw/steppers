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
| "fun"  { FUN }
| "->"   { RIGHT }
| "{"    { LBRACE }
| "return" { RETURN }
| ","    { COMMA }
| ";"    { SEMI }
| "}"    { RBRACE }
| "with" { WITH }
| "handle" { HANDLE }
| digit+
         { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| (lower | '_')+(alpha | digit | '_')*
  	 { VAR (Lexing.lexeme lexbuf) }
| upper+(alpha | digit | '_')*
         { OP (Lexing.lexeme lexbuf) }
| eof	 { EOF }                (* 入力終了 *)
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }
