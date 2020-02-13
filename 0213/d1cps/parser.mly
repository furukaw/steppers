%{
open Syntax
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token FUN RIGHT
%token LBRACE RETURN COMMA SEMI RBRACE WITH HANDLE
%token <int> NUMBER
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
%nonassoc WITH HANDLE
%right FUN RIGHT
%nonassoc OP
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

expr:
| simple_expr
        { $1 }
| value
        { Val ($1) }
| simple_expr simple_expr
        { App ($1, $2) }
| OP simple_expr
        { Op ($1, $2) }
| WITH LBRACE handler RBRACE HANDLE expr
        { With ($3, $6) }

value:
| simple_value
        { $1 }
| FUN VAR RIGHT expr
        { Fun ($2, $4) }

simple_value:
| VAR
        { Var ($1) }

simple_expr:
| LPAREN expr RPAREN
        { $2 }
| simple_value
        { Val ($1) }

handler:
| ret_opt COMMA ops
        { {return = $1; ops = $3} }
| ret_opt
        { {return = $1; ops = []} }

ret_opt:
| ret
        { Some ($1) }
|
        { None }

ret:
| RETURN VAR RIGHT expr
        { ($2, $4) }

ops:
| op
        { [$1] }
| op ops
        { $1 :: $2 }

op:
| OP LPAREN VAR SEMI VAR RPAREN RIGHT expr
        { ($1, $3, $5, $8) }
