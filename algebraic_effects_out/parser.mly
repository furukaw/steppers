%{
open Syntax
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token TRUE FALSE FUN RIGHT HANDLER LBRACK RBRACK RETURN COMMA SEMI IF THEN ELSE WITH HANDLE
%token <string> OP
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token <string> VAR
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.c_t> com

/* 開始記号の定義 */
%start com

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%nonassoc COMMA
%right FUN RIGHT
%nonassoc HANDLER
%nonassoc WITH HANDLE
%right IF THEN ELSE
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_value:
| VAR
        { Var ($1) }
| TRUE
        { True }
| FALSE
        { False }

not_simple_value:
| FUN VAR RIGHT com
        { Fun ($2, $4) }
| HANDLER LBRACK handler RBRACK
        { $3 }

handler:
|       { Handler (None, []) }
| return_handler
        { Handler (Some $1, []) }
| op_handlers
        { Handler (None, List.rev $1) }
| return_handler op_handlers
        { Handler (Some $1, List.rev $2) }

return_handler:
| RETURN VAR RIGHT com
        { ($2, $4) }

op_handlers:
| op_handler
        { [$1] }
| op_handler op_handlers
        { $1 :: $2 }

op_handler:
| OP LPAREN VAR SEMI VAR RPAREN RIGHT com
        { ($1, $3, $5, $8) }

com:
| simple_com
        { $1 }
| not_simple_value
        { Val ($1) }
| simple_com simple_com
        { App ($1, $2) }
| IF com THEN com ELSE com
        { If ($2, $4, $6) }
| OP simple_com
        { Op ($1, $2) }
| WITH com HANDLE com
        { With ($2, $4) }

simple_com:
| LPAREN com RPAREN
        { $2 }
| simple_value
        { Val ($1) }