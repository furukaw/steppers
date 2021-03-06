%{
open Syntax
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token TRUE FALSE FUN RIGHT HANDLER LBRACK RBRACK COMMA RETURN SEMI DOT DO LEFT IN IF THEN ELSE WITH HANDLE
%token READ PRINT
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
%nonassoc SEMI DOT
%nonassoc LEFT
%right FUN RIGHT
%nonassoc HANDLER
%nonassoc WITH HANDLE
%right DO IN
%right IF THEN ELSE
%right RETURN
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

core_value:
| VAR
        { Var ($1) }
| TRUE
        { True }
| FALSE
        { False }

value_without_pq:
| core_value
        { $1 }
| FUN VAR RIGHT com
        { Fun ($2, $4) }
| HANDLER LBRACK handler RBRACK
        { Handler ($3) }

simple_value:
| core_value
        { $1 }
| LPAREN value_without_pq RPAREN
        { $2 }

value:
| value_without_pq
        { $1 }
| LPAREN value_without_pq RPAREN
        { $2 }

com:
| value
        { Return ($1) }
| RETURN value
        { Return ($2) }
| op LPAREN value SEMI VAR DOT com RPAREN
        { Op ($1, $3, $5, $7) }
| DO VAR LEFT com IN com
        { Do ($2, $4, $6) }
| IF value THEN com ELSE com
        { If ($2, $4, $6) }
| simple_value simple_value
        { App ($1, $2) }
| WITH value HANDLE com
        { With ($2, $4) }

handler:
| handler_return COMMA handler_ops
        { (Some $1, List.rev $3) }
| handler_ops
        { (None, List.rev $1) }

handler_return:
| RETURN VAR RIGHT com
        { ($2, $4) }

handler_ops:
| handler_op
        { [$1] }
| handler_op COMMA handler_ops
        { $1 :: $3 }

handler_op:
| op LPAREN VAR SEMI VAR RPAREN RIGHT com
        { ($1, $3, $5, $8) }

op:
| READ
        { Read }
| PRINT
        { Print }
