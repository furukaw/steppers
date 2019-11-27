open Syntax
open Util
open Memo

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) (cont_in : cont_in)
    (cont_out : cont_out) : v =
  match e with
  | Val (v) -> apply_in cont_in v cont_out
  | Fun (x, e1) -> apply_in cont_in (VFun (x, e1)) cont_out
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out)
      (FApp2 (e1, (ctxt_in, ctxt_out), cont_in)) cont_out
  | Shift (x, e1) ->
    let (ctxt_next, ctxt_outer) = match ctxt_out with
      | [] -> failwith "no reset around shift"
      | (CReset (first)) :: rest -> (first, rest) in
    let new_var = gen_var_name () in
    let new_var_in_ctxt = plug_in_reset (Val (Var (new_var))) ctxt_in in
    let x_cont_in = VCont (cont_in, (new_var, Reset (new_var_in_ctxt))) in
    let reduct = Reset (subst e1 [(x, x_cont_in)]) in
    eval reduct (ctxt_next, ctxt_outer) FId cont_out
  | Reset (e1) ->
    eval e1 ([], add_reset ctxt_in ctxt_out) FId (GReset (cont_in, cont_out))

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out): v =
  match cont_in with
  | FId -> apply_out cont_out v
  | FApp2 (e1, (ctxt_in, ctxt_out), cont_in) -> let v2 = v in
    eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out)
      (FApp1 (v2, (ctxt_in, ctxt_out), cont_in)) cont_out
  | FApp1 (v2, (ctxt_in, ctxt_out), cont_in) -> let v1 = v in
    begin
      match v1 with
      | VFun (x, e_fun) ->
        let reduct = subst e_fun [(x, v2)] in
        eval reduct (ctxt_in, ctxt_out) cont_in cont_out
      | VCont (cont_in', (x, e_fun)) ->
        apply_in cont_in' v2 (GReset (cont_in, cont_out))
      | _ -> failwith "type error"
    end

and apply_out (cont_out : cont_out) (v : v) : v = match cont_out with
  | GId -> v
  | GReset (cont_in, cont_out) ->
    apply_in cont_in v cont_out

let stepper (e : e) : v =
  eval e ([], []) FId GId
