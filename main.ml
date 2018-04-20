(* メイン関数 *)
let rec go (state : State.t) : unit =
  State.print state;
  try
  let input = Parser.expr Lexer.token (Lexing.from_channel stdin) in
  let new_state = Operate.eval input state in
  go new_state
  with Parsing.Parse_error -> print_endline "Parse error."; go state
     | Failure x -> print_endline ("error: " ^ x); go state

(* スタートアップ *)
let _ = go State.Nothing
