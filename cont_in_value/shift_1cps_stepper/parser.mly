%{
open Syntax
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token FUN ARROW PLUS SHIFT RESET SHIFT0 CONTROL CONTROL0
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token <string> VAR
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%right SHIFT LAM DOT
%left PLUS
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| VAR
        { Var ($1) }
| LPAREN expr RPAREN
        { $2 }

expr:
| simple_expr
        { $1 }
| FUN VAR ARROW expr
        { Fun ($2, $4) }
| expr simple_expr
        { App ($1, $2) }
| SHIFT LPAREN FUN VAR ARROW expr RPAREN
        { Shift ($4, $6) }
| RESET LPAREN FUN LPAREN RPAREN ARROW expr RPAREN
        { Reset ($7) }
