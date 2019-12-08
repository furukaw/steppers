open Syntax
open Util
open Memo

let id_out : cont_out = fun v -> v

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt)
    (cont_in : cont_in) (cont_out : cont_out) : v =
  match e with
  | Val (v) -> apply_in cont_in v ctxt_out cont_out
  | App (e1, e2) ->
    eval e2 ((CApp2 e1) :: ctxt_in, ctxt_out)
      (FApp2 (e1, ctxt_in, cont_in)) cont_out
  | Shift (x, e1) ->
    begin match ctxt_out with
      | [] -> failwith "no reset around shift"
      | CReset (ctxt_in') :: ctxt_out' ->
        let redex = Reset (plug_in_reset e ctxt_in) in
        let new_var = gen_var_name () in
        let x_cont_in = Cont (new_var, ctxt_in, cont_in) in
        let reduct = Reset (subst e1 [(x, x_cont_in)]) in
        memo redex reduct (ctxt_in', ctxt_out');
        eval reduct (ctxt_in', ctxt_out') FId cont_out
    end
  | Reset (e1) ->
    eval e1 ([], CReset ctxt_in :: ctxt_out) FId (fun v ->
        let redex = Reset (Val v) in
        let reduct = Val v in
        memo redex reduct (ctxt_in, ctxt_out);
        apply_in cont_in v ctxt_out cont_out
      )

and apply_in (cont_in : cont_in) (v : v) (ctxt_out : ctxt_out)
    (cont_out : cont_out): v =
  match cont_in with
  | FId ->
    cont_out v
  | FApp2 (e1, ctxt_in2, cont_in2) -> let v2 = v in
    eval e1 (CApp1 (v2) :: ctxt_in2, ctxt_out)
      (FApp1 (v2, ctxt_in2, cont_in2)) cont_out
  | FApp1 (v2, ctxt_in1, cont_in1) -> let v1 = v in
    begin
      match v1 with
      | Fun (x, e_fun) ->
        let redex = App (Val v1, Val v2) in
        let reduct = subst e_fun [(x, v2)] in
        memo redex reduct (ctxt_in1, ctxt_out);
        eval reduct (ctxt_in1, ctxt_out) cont_in1 cont_out
      | Cont (x, ctxt_in', cont_in') ->
        let redex = App (Val v1, Val v2) in
        let reduct = Reset (plug_in_reset (Val v2) ctxt_in') in
        memo redex reduct (ctxt_in1, ctxt_out);
        apply_in cont_in' v2 (CReset [] :: ctxt_out) (fun v ->
            apply_in cont_in v ctxt_out cont_out
          )
      | _ -> failwith "type error"
    end

let stepper (e : e) : v =
  eval e ([], []) FId id_out
