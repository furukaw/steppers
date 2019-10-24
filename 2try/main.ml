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
  let result = Eval.eval program in
  print_string "Result:  ";
  match result with
  | Ok v -> Syntax.print_v v
  | Error v -> Syntax.print_e (Syntax.Raise (Syntax.Val v))

(* スタートアップ *)
let _ = go ()
