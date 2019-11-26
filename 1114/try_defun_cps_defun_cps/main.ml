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
  let result = Eval.interpreter program in
  print_string "Result:  ";
  match result with
  | Syntax.Value v ->
    Syntax.print_v v
  | Syntax.Raised (v) ->
    print_string "Exception: ";
    Syntax.print_v v

(* スタートアップ *)
let _ = go ()
