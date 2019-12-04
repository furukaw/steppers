open Syntax
open Util
open Memo

let id : cont = fun v _ -> v

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) (cont : cont) : v =
  print_newline ();
  print_endline "eval";
  print_string "式 : ";
  print_e e;
  print_ctxt (ctxt_in, ctxt_out);
  match e with
  | Val (v) -> cont v ctxt_out
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out) (fun v2 ctxt_out ->
        print_newline ();
        print_endline "App2";
        print_string "値 : ";
        print_v v2;
        print_ctxt (ctxt_in, ctxt_out);
        eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out) (fun v1 ctxt_out ->
            print_newline ();
            print_endline "App1";
            print_string "値 : ";
            print_v v1;
            print_ctxt (ctxt_in, ctxt_out);
            match v1 with
            | Fun (x, e_fun) ->
              let redex = App (Val v1, Val v2) in
              let reduct = subst e_fun [(x, v2)] in
              memo redex reduct (ctxt_in, ctxt_out);
              eval reduct (ctxt_in, ctxt_out) cont
            | Cont (cont', ctxt_out', (x, e_fun)) ->
              let redex = App (Val v1, Val v2) in
              let reduct = subst e_fun [(x, v2)] in
              memo redex reduct (ctxt_in, ctxt_out);
              cont (cont' v2 ctxt_out) ctxt_out'
            | _ -> failwith "type error"
          )
      )
  | Shift (x, e1) ->
    let (ctxt_next, ctxt_outer) = match ctxt_out with
      | [] -> failwith "no reset around shift"
      | (CReset (first)) :: rest -> (first, rest) in
    let redex = Reset (plug_in_reset e ctxt_in) in
    let new_var = gen_var_name () in
    let new_var_in_ctxt = plug_in_reset (Val (Var (new_var))) ctxt_in in
    let x_cont = Cont (cont, add_reset [] ctxt_outer, (new_var, Reset (new_var_in_ctxt))) in
    let reduct = Reset (subst e1 [(x, x_cont)]) in
    memo redex reduct (ctxt_next, ctxt_outer);
    eval reduct (ctxt_next, ctxt_outer) id
  | Reset (e1) ->
    cont (eval e1 ([], add_reset ctxt_in ctxt_out) (fun v ctxt_out ->
        print_newline ();
        print_endline "Reset";
        print_string "値 : ";
        print_v v;
        print_ctxt (ctxt_in, ctxt_out);
        memo (Reset (Val v)) (Val v) (ctxt_in, ctxt_out);
        v
      )) ctxt_out

let stepper (e : e) : v =
  eval e ([], []) (fun v _ ->
      (* memo (Reset (Val v)) (Val v) ([], []); *)
      v
    )
