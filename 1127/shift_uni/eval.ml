open Syntax
open Util
open Memo

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) (cont_in : cont_in) (cont_out : cont_out) : v =
  match e with
  | Val (v) -> apply_in cont_in v cont_out
  | Fun (x, e1) -> apply_in cont_in (VFun (x, e1)) cont_out
  | App (e1, e2) ->
    eval e2 (FApp2 (e1, cont_in, cont_out)) cont_out
  | Shift (x, e1) ->
    begin match cont_out with
      | GId -> failwith "no reset around shift"
      | (GReset (cont_in', cont_out')) ->
        let redex = Reset (plug_in_reset e cont_in) in
        let new_var = gen_var_name () in
        let x_cont_in =
          VCont (cont_in, cont_out, new_var) in
        let reduct = Reset (subst e1 [(x, x_cont_in)]) in
        memo redex reduct (cont_in', cont_out');
        eval reduct cont_in' cont_out'
    end
  | Reset (e1) ->
    eval e1 FId (GReset (cont_in, cont_out))

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out): v =
  match cont_in with
  | FId ->
    apply_out cont_out v
  | FApp2 (e1, cont_in2, cont_out2) -> let v2 = v in
    eval e1 (FApp1 (v2, cont_in2, cont_out2)) cont_out
  | FApp1 (v2, cont_in1, cont_out1) -> let v1 = v in
    begin
      match v1 with
      | VFun (x, e_fun) ->
        let redex = App (Val v1, Val v2) in
        let reduct = subst e_fun [(x, v2)] in
        memo redex reduct (cont_in1, cont_out);
        eval reduct cont_in1 cont_out
      | VCont (cont_in', cont_out', x) ->
        let redex = App (Val v1, Val v2) in
        let reduct = Reset (plug_in_reset (Val v2) cont_in') in
        memo redex reduct (cont_in1, cont_out);
        apply_in cont_in' v2 (GReset (cont_in1, cont_out1))
      | _ -> failwith "type error"
    end

and apply_out (cont_out : cont_out) (v : v) : v =
  match cont_out with
  | GId ->
    v
  | GReset (cont_in', cont_out') ->
    let redex = Reset (Val v) in
    let reduct = Val v in
    memo redex reduct (cont_in', cont_out');
    apply_in cont_in' v cont_out'

let stepper (e : e) : v =
  eval e FId GId
