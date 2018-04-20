%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token ADD REDUCE PARENT FREE VARIABLES IS CLOSED RENAME RESET LAMBDA DOT
%token AND EQUAL ALPHA EQ CONVERSION
%token <string> VAR
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr
%type <Syntax.lam> lam

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%left ADD
%left DOT
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| VAR
	{ Syntax.Var ($1) }
| LPAREN lam RPAREN
	{ $2 }

abs:
| VAR DOT lam
      	{ Syntax.Abs ($1, [], $3, true) }
| VAR abs
	{ Syntax.Abs ($1, [], $2, true) }

app:
| simple_expr simple_expr
	{ Syntax.App ($1, $2, true) }
| simple_expr LAMBDA abs
  	{ Syntax.App ($1, $3, true) }
| app simple_expr
  	{ Syntax.App ($1, $2, true) }
| app LAMBDA abs
      	{ Syntax.App ($1, $3, true) }

lam:
| simple_expr
	{ $1 }
| LAMBDA abs
  	{ $2 }
| app
	{ $1 }

expr:
| lam
	{ Syntax.Lam ($1) }
| ADD PARENT
        { Syntax.Max }
| REDUCE PARENT
        { Syntax.Min }
| FREE VARIABLES
        { Syntax.FV }
| IS CLOSED
	{ Syntax.IsClosed }
| RENAME VAR VAR
  	{ Syntax.Rename ($2, $3) }
| AND lam
	{ Syntax.And ($2) }
| EQUAL ALPHA
  	{ Syntax.AlphaEq }
| ALPHA EQ
        { Syntax.AlphaEq }
| ALPHA CONVERSION
        { Syntax.AlphaEq }
| RESET
	{ Syntax.Reset }
