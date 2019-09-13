open Syntax
open Util
open Context

(* shift 簡約後に継続を捨てるための例外、引数の式は shift で簡約された部分の式 *)
exception Shift of e_t

(* 式とコンテキストの情報を受け取って評価して値を返す *)
(* 簡約ごとに簡約の内容を標準出力する *)
(* shift の簡約をしたら例外 Shift (その時点のプログラム全体) を起こす *)
let rec f (expr : e_t) (ctxt : c_t) : v_t =
  match expr with
  | Value (v) -> v
  | Var (x) -> failwith ("unbound variable: " ^ x)
  | App (e1, e2) ->
    begin
      let v2 = f e2 (add ctxt (CAppR e1)) in
      let v1 = f e1 (add ctxt (CAppL v2)) in
      match v1 with
      | Lam (x, e) ->
        let e' = subst e x v2 in
        memo (App (Value (v1), Value (v2))) e' ctxt;
        let v = f e' ctxt in
        v
      | _ -> failwith "not a function"
    end
  | Plus (e1, e2) ->
    begin
      let v2 = f e2 (add ctxt (CPlusR e1)) in
      let v1 = f e1 (add ctxt (CPlusL v2)) in
      match (v1, v2) with
      | (Num n1, Num n2) ->
        let v = Num (n1 + n2) in
        memo (Plus (Value (v1), Value (v2))) (Value v) ctxt;
        v
      | _ -> failwith "not a integer"
    end
  | Reset (e) ->
    begin
      try
        let v = f e (add_reset ctxt) in
        memo (Reset (Value v)) (Value v) ctxt;
        v
      with Shift (e) -> f e ctxt
    end
  | Shift (k, e) ->
    let hole_var = gen_var_name () in
    let (ctxt_inside_reset, ctxt_outside_reset) = match ctxt with
      | CHole (frames) -> (frames, CHole ([]))
      | CReset (frames, outer_ctxt) -> (frames, outer_ctxt) in
    let redex = Reset (plug_in_reset expr ctxt_inside_reset) in
    let e1 = Value (Lam (k, e)) in
    let continuation = plug_in_reset (Var hole_var) ctxt_inside_reset in
    let e2 = Value (Lam (hole_var, Reset (continuation))) in
    let reduct = Reset (App (e1, e2)) in
    memo redex reduct ctxt_outside_reset;
    raise (Shift reduct)
  | Shift0 (k, e) ->
    let hole_var = gen_var_name () in
    let (ctxt_inside_reset, ctxt_outside_reset) = match ctxt with
      | CHole (frames) -> (frames, CHole ([]))
      | CReset (frames, outer_ctxt) -> (frames, outer_ctxt) in
    let redex = Reset (plug_in_reset expr ctxt_inside_reset) in
    let e1 = Value (Lam (k, e)) in
    let continuation = plug_in_reset (Var hole_var) ctxt_inside_reset in
    let e2 = Value (Lam (hole_var, Reset (continuation))) in
    let reduct = App (e1, e2) in
    memo redex reduct ctxt_outside_reset;
    raise (Shift reduct)
  | Control (k, e) ->
    let hole_var = gen_var_name () in
    let (ctxt_inside_reset, ctxt_outside_reset) = match ctxt with
      | CHole (frames) -> (frames, CHole ([]))
      | CReset (frames, outer_ctxt) -> (frames, outer_ctxt) in
    let redex = Reset (plug_in_reset expr ctxt_inside_reset) in
    let e1 = Value (Lam (k, e)) in
    let continuation = plug_in_reset (Var hole_var) ctxt_inside_reset in
    let e2 = Value (Lam (hole_var,  continuation)) in
    let reduct = Reset (App (e1, e2)) in
    memo redex reduct ctxt_outside_reset;
    raise (Shift reduct)
  | Control0 (k, e) ->
    let hole_var = gen_var_name () in
    let (ctxt_inside_reset, ctxt_outside_reset) = match ctxt with
      | CHole (frames) -> (frames, CHole ([]))
      | CReset (frames, outer_ctxt) -> (frames, outer_ctxt) in
    let redex = Reset (plug_in_reset expr ctxt_inside_reset) in
    let e1 = Value (Lam (k, e)) in
    let continuation = plug_in_reset (Var hole_var) ctxt_inside_reset in
    let e2 = Value (Lam (hole_var, continuation)) in
    let reduct = App (e1, e2) in
    memo redex reduct ctxt_outside_reset;
    raise (Shift reduct)

(* 式を受け取って空のコンテキストで評価を始める、shift が起こったらその時の式でやり直す *)
let rec stepper (expr : e_t) : v_t =
  try
    f expr (CHole ([]))
  with Shift _ -> failwith "shift is executed without enclosing reset"
