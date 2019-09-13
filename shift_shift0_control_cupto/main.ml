(* メイン関数 *)
let go () =
  let program = Parser.expr Lexer.token (Lexing.from_channel stdin) in
  (* これで標準入力を字句解析して、構文解析した結果を program に入れ *)
  print_string "Parsed :";
  Syntax.print_exp program;		(* 入力を表示する *)
  print_newline ();
  Util.record_var_name program;
  let result = Eval.stepper program in
  print_string "Result :";
  Syntax.print_value result;
  print_newline ()

(* スタートアップ *)
let _ = go ()
