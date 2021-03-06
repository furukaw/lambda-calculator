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
| "add"  { ADD }
| "and"  { AND }
| "parent" "hesis"? { PARENT }
| "reduce" { REDUCE }
| "free" { FREE }
| "variable" "s"? { VARIABLES }
| "is"   { IS }
| "closed" { CLOSED }
| "rename" { RENAME }
| "="    { EQUAL }
| "alpha" | "α" {ALPHA}
| "eq" "uivalence"? { EQ }
| "conversion" { CONVERSION }
| "reset" { RESET }
| "l"	 { LAMBDA }
| "."	 { DOT }
| lower
  	 { VAR (Lexing.lexeme lexbuf) }
| eof	 { EOF }                (* 入力終了 *)
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }
