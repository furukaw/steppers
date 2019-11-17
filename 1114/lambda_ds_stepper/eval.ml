open Syntax
open Util
open Context

(* 簡約ステップを出力しながら式を実行する *)
let rec f (e : e) (ctxt : ctxt) : v = match e with
  | Val (v) -> v
  | Fun (x, e1) -> VFun (x, e1)
  | App (e1, e2) ->
    begin
      let v1 = f e1 (add (CAppL (e2)) ctxt) in
      let v2 = f e2 (add (CAppR (v1)) ctxt) in
      let redex = App (Val (v1), Val (v2)) in
      let reduct = match v1 with
        | VFun (x, e_fun) -> subst e_fun [(x, v2)]
        | _ -> failwith "type error" in
      memo redex reduct ctxt;
      let result = f reduct ctxt in
      result
    end
