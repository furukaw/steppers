open Context

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
  try
    let result = Eval.stepper program in
    print_string "Result:  ";
    Syntax.print_a result
  with
  | Eval.TypeError (s, e) ->
    print_string "type error (";
    print_string s;
    print_string "): ";
    Syntax.print_e e

(* スタートアップ *)
let _ = go ()
