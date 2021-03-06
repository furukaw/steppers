(* メイン関数 *)
let go () =
  let program = Parser.com Lexer.token (Lexing.from_channel stdin) in
  if Array.length Sys.argv = 1
  then begin
    print_string "Parsed:  ";
    Syntax.print_computation program;		(* 入力を表示する *)
    Util.record_var_name program;
  end;
  let result = Eval.f program [] in
  print_string "Result:  ";
  Syntax.print_computation result

(* スタートアップ *)
let _ = go ()
