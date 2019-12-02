open Syntax
open Util
open Memo

let id : cont = fun v -> v

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) (cont : cont) : v =
  match e with
  | Val (v) -> cont v
  | Fun (x, e1) -> cont (VFun (x, e1))
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out) (fun v2 ->
        eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out) (fun v1 ->
            match v1 with
            | VFun (x, e_fun) ->
              let redex = App (Val v1, Val v2) in
              let reduct = subst e_fun [(x, v2)] in
              memo redex reduct (ctxt_in, ctxt_out);
              eval reduct (ctxt_in, ctxt_out) cont
            | VCont (cont', (x, e_fun)) ->
              let redex = App (Val v1, Val v2) in
              let reduct = subst e_fun [(x, v2)] in
              memo redex reduct (ctxt_in, ctxt_out);
              cont (cont' v2)
            | _ -> failwith "type error"
          )
      )
  | Shift0 (x, e1) ->
    let (ctxt_next, ctxt_outer) = match ctxt_out with
      | [] -> failwith "no reset0 around shift0"
      | (CReset0 (first)) :: rest -> (first, rest) in
    let redex = Reset0 (plug_in_reset0 e ctxt_in) in
    let new_var = gen_var_name () in
    let new_var_in_ctxt = plug_in_reset0 (Val (Var (new_var))) ctxt_in in
    let x_cont = VCont (cont, (new_var, Reset0 (new_var_in_ctxt))) in
    let reduct = subst e1 [(x, x_cont)] in
    memo redex reduct (ctxt_next, ctxt_outer);
    eval reduct (ctxt_next, ctxt_outer) id
  | Reset0 (e1) ->
    cont (eval e1 ([], add_reset0 ctxt_in ctxt_out) (fun v ->
        memo (Reset0 (Val v)) (Val v) (ctxt_in, ctxt_out);
        v
      ))

let stepper (e : e) : v =
  eval e ([], []) (fun v ->
        (* memo (Reset0 (Val v)) (Val v) ([], []); *)
        v
      )
