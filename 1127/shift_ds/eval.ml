open Syntax
open Util
open Memo

exception Shifting of e

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) : v =
  match e with
  | Val (v) -> v
  | App (e1, e2) ->
    begin
      let v2 = eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out) in
      let v1 = eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out) in
      match v1 with
      | Fun (x, e_fun) ->
        let redex = App (Val v1, Val v2) in
        let reduct = subst e_fun [(x, v2)] in
        memo redex reduct (ctxt_in, ctxt_out);
        eval reduct (ctxt_in, ctxt_out)
      | Cont (x, e_fun) ->
        let redex = App (Val v1, Val v2) in
        let reduct = subst e_fun [(x, v2)] in
        memo redex reduct (ctxt_in, ctxt_out);
        eval reduct (ctxt_in, ctxt_out)
      | _ -> failwith "type error"
    end
  | Shift (x, e1) ->
    let redex = Reset (plug_in_reset e ctxt_in) in
    let new_var = gen_var_name () in
    let x_val = Cont (new_var,
                      Reset (plug_in_reset (Val (Var new_var)) ctxt_in)) in
    let reduct = Reset (subst e1 [(x, x_val)]) in
    let (ctxt_in', ctxt_out') = match ctxt_out with
      | [] -> failwith "no reset around shift"
      | (CReset (ctxt_in')) :: ctxt_out' -> (ctxt_in', ctxt_out') in
    memo redex reduct (ctxt_in', ctxt_out');
    raise (Shifting (reduct))
  | Reset (e1) ->
    begin try
        let v1 = eval e1 ([], add_reset ctxt_in ctxt_out) in
        memo (Reset (Val v1)) (Val v1) (ctxt_in, ctxt_out);
        v1
      with Shifting (e2) ->
        eval e2 (ctxt_in, ctxt_out)
    end

let stepper (e : e) : v =
  eval e ([], [])
