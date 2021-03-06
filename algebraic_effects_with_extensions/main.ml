let use_stdin () =
  Op.mode := Op.CUI

let use_file (filename : string) =
  Op.mode := Op.Test;
  Op.channel := (open_in filename)

(* メイン関数 *)
let go () =
  let program = Parser.com Lexer.token (Lexing.from_channel stdin) in
  let arg_num = Array.length Sys.argv - 1 in
  if arg_num = 0
  then begin
    use_stdin ();
    print_string "Parsed:  ";
    Syntax.print_computation program;		(* 入力を表示する *)
    print_newline ();
    Util.record_var_name program;
  end
  else if arg_num = 1
  then use_stdin ()
  else use_file (Sys.argv.(2));
  try
    let result = Eval.f program [] in
    print_string "Result:  ";
    Syntax.print_computation result
  with
    Op.Raise s -> print_endline ("Error: " ^ s)

(* スタートアップ *)
let _ = go ()
