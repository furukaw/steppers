open Syntax
open Util
open Memo

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) (cont : cont) : v =
  match e with
  | Val (v) -> apply cont v
  | Fun (x, e1) -> apply cont (VFun (x, e1))
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out)
      (FApp2 (e1, (ctxt_in, ctxt_out), cont))
  | Shift (x, e1) ->
    let (ctxt_next, ctxt_outer) = match ctxt_out with
      | [] -> failwith "no reset around shift"
      | (CReset (first)) :: rest -> (first, rest) in
    let new_var = gen_var_name () in
    let new_var_in_ctxt = plug_in_reset (Val (Var (new_var))) ctxt_in in
    let x_cont = VCont (cont, (new_var, Reset (new_var_in_ctxt))) in
    let reduct = Reset (subst e1 [(x, x_cont)]) in
    eval reduct (ctxt_next, ctxt_outer) FId
  | Reset (e1) ->
    apply cont (eval e1 ([], add_reset ctxt_in ctxt_out) FId)

and apply (cont : cont) (v : v) : v = match cont with
  | FId -> v
  | FApp2 (e1, (ctxt_in, ctxt_out), cont) -> let v2 = v in
    eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out)
      (FApp1 (v2, (ctxt_in, ctxt_out), cont))
  | FApp1 (v2, (ctxt_in, ctxt_out), cont) -> let v1 = v in
    begin
      match v1 with
      | VFun (x, e_fun) ->
        let reduct = subst e_fun [(x, v2)] in
        eval reduct (ctxt_in, ctxt_out) cont
      | VCont (cont', (x, e_fun)) ->
        apply cont (apply cont' v2)
      | _ -> failwith "type error"
    end

let stepper (e : e) : v =
  eval e ([], []) FId
