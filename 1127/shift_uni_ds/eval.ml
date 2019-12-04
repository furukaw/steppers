open Syntax
open Util
open Memo

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) 
    (cont_in : cont_in) (cont_out : cont_out) : a =
  match e with
  | Val (v) -> apply_in cont_in v cont_out
  | App (e1, e2) ->
    eval e2 
      (FApp2 (e1, cont_in, cont_out)) cont_out
  | Shift (x, e1) ->
    let redex = Reset (plug_in_reset e cont_in) in
    let new_var = gen_var_name () in
    let x_val = Cont (new_var,
                      Reset (plug_in_reset (Val (Var new_var)) cont_in)) in
    let reduct = Reset (subst e1 [(x, x_val)]) in
    let (cont_in', cont_out') = match cont_out with
      | GId -> failwith "no reset around shift"
      | (GReset (cont_in', cont_out')) -> (cont_in', cont_out') in
    memo redex reduct (cont_in', cont_out');
    apply_out cont_out (Shifting (reduct))
  | Reset (e1) ->
    eval e1  FId
      (GReset (cont_in, cont_out))

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out) : a =
  match cont_in with
  | FId -> apply_out cont_out (Value v)
  | FApp2 (e1, cont_in, cont_out) -> let v2 = v in
    eval e1 
      (FApp1 (v2, cont_in, cont_out)) cont_out
  | FApp1 (v2, cont_in, cont_out) -> let v1 = v in
    begin
      match v1 with
      | Fun (x, e_fun) ->
        let redex = App (Val v1, Val v2) in
        let reduct = subst e_fun [(x, v2)] in
        memo redex reduct (cont_in, cont_out);
        eval reduct  cont_in cont_out
      | Cont (x, e_fun) ->
        let redex = App (Val v1, Val v2) in
        let reduct = subst e_fun [(x, v2)] in
        memo redex reduct (cont_in, cont_out);
        eval reduct cont_in cont_out
      | _ -> failwith "type error"
    end

and apply_out (cont_out : cont_out) (a : a) : a = match cont_out with
  | GId -> a
  | GReset (cont_in, cont_out) -> let a1 = a in
    begin
      match a1 with
      | Value v1 ->
        memo (Reset (Val v1)) (Val v1) (cont_in, cont_out);
        apply_in cont_in v1 cont_out
      | Shifting (e2) ->
        eval e2 cont_in cont_out
    end

let stepper (e : e) : (v, unit) result =
  match eval e FId GId with
  | Value v -> Ok v
  | Shifting e -> Error ()
