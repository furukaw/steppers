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
  let result = Eval.eval program in
  begin match result with
    | Eval.Value v ->
      print_string "Result:  ";
      Syntax.print_v v
  | Eval.OpCall (op, v, ctxt) ->
    print_endline "Error: no handler for op";
    Syntax.print_e (Context.plug (Syntax.Op (op, Syntax.Val v)) ctxt)
  end
(* スタートアップ *)
let _ = go ()
