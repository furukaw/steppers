%{
open Syntax
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token FUN RIGHT HANDLER LBRACK RBRACK RETURN COMMA SEMI WITH HANDLE
%token <string> VAR
%token <string> OP
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.e> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%nonassoc COMMA
%right WITH HANDLE
%nonassoc RETURN
%right FUN RIGHT
%nonassoc HANDLER
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

expr:
| LPAREN expr RPAREN
        { $2 }
| value
        { Val ($1) }
| simple_expr simple_expr
        { App ($1, $2) }
| OP simple_expr
        { Op ($1, $2) }
| WITH expr HANDLE expr
        { With ($2, $4) }

value:
| simple_value
        { $1 }
| FUN VAR RIGHT expr
        { Fun ($2, $4) }
| HANDLER LBRACK handler RBRACK
        { Handler ($3) }

handler:
| return_handler COMMA ops_handler
        { {return = Some $1; ops = List.rev $3} }
| return_handler
        { {return = Some $1; ops = []} }
| ops_handler
        { {return = None; ops = List.rev $1} }
|       { {return = None; ops = []} }

return_handler:
| RETURN VAR RIGHT expr
        { ($2, $4) }

ops_handler:
| op_handler
        { [$1] }
| op_handler ops_handler
        { $1 :: $2 }

op_handler:
| OP LPAREN VAR SEMI VAR RPAREN RIGHT expr
        { ($1, $3, $5, $8) }

simple_value:
| VAR
        { Var ($1) }

simple_expr:
| LPAREN expr RPAREN
        { $2 }
| simple_value
        { Val ($1) }
