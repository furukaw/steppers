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
  Util.record_var_name program;
  let result = Eval.interpreter program in
  match result with
  | Syntax.Return v -> 
    print_string "Result:  ";
    Syntax.print_v v
  | Syntax.OpCall (op, _, _, _) ->
    print_endline ("Error: no handlers for " ^ op)

(* スタートアップ *)
let _ = go ()
