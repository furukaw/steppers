open Syntax
open Util
open Memo

let id : cont = fun v -> Value v

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) (cont : cont) : a =
  match e with
  | Val (v) -> cont v
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out) (fun v2 ->
        eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out) (fun v1 ->
            match v1 with
            | Fun (x, e_fun) ->
              let redex = App (Val v1, Val v2) in
              let reduct = subst e_fun [(x, v2)] in
              memo redex reduct (ctxt_in, ctxt_out);
              eval reduct (ctxt_in, ctxt_out) cont
            | Cont (x, e_fun) ->
              let redex = App (Val v1, Val v2) in
              let reduct = subst e_fun [(x, v2)] in
              memo redex reduct (ctxt_in, ctxt_out);
              eval reduct (ctxt_in, ctxt_out) cont
            | _ -> failwith "type error"
          )
      )
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
    Shifting (reduct)
  | Reset (e1) ->
    begin
      let a1 = eval e1 ([], add_reset ctxt_in ctxt_out) id in
      match a1 with
      | Value v1 ->
        memo (Reset (Val v1)) (Val v1) (ctxt_in, ctxt_out);
        cont v1
      | Shifting (e2) ->
        eval e2 (ctxt_in, ctxt_out) cont
    end

let stepper (e : e) : (v, unit) result =
  match eval e ([], []) id with
  | Value v -> Ok v
  | Shifting e -> Error ()
