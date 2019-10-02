open Syntax
open Util
open Context

(* 簡約ステップを出力しながら式を実行する *)
let rec f (e : e_t) (ctxt : ctx_t) : v_t = match e with
  | Val (v) -> v
  | App (e1, e2) ->
    begin
      let v1 = f e1 (add (CApp1 (e2)) ctxt) in
      let v2 = f e2 (add (CApp2 (v1)) ctxt) in
      let redex = App (Val (v1), Val (v2)) in
      let reduct = match v1 with
        | Fun (x, e_fun) -> subst e_fun (x, v2)
        | _ -> failwith "type error" in
      memo redex reduct ctxt;
      let result = f reduct ctxt in
      result
    end
  | If (e0, e1, e2) ->
    let v0 = f e0 (add (CIf (e1, e2)) ctxt) in
    let reduct = match v0 with
      | True -> e1
      | False -> e2
      | _ -> failwith "type error" in
    memo e reduct ctxt;
    let result = f reduct ctxt in
    result
