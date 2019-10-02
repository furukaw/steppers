(* メイン関数 *)
let go () =
  let program = Parser.expr Lexer.token (Lexing.from_channel stdin) in
  let arg_num = Array.length Sys.argv - 1 in
  if arg_num = 0
  then begin
    print_string "Parsed:  ";
    Syntax.print_e program;		(* 入力を表示する *)
    print_newline ();
  end;
  let result = Eval.f program [] in
  print_string "Result:  ";
  Syntax.print_v result

(* スタートアップ *)
let _ = go ()
