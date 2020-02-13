open Syntax
open Util
open Context

(* 簡約ステップを出力しながら式を実行する *)
let rec f (e : e_t) (ctxt : ctx_t) : v_t = match e with
  | Val (v) -> v
  | App (e1, e2) ->
    (let v1 = f e1 (add (CAppL (e2)) ctxt) in
     let v2 = f e2 (add (CAppR (v1)) ctxt) in
     let redex = App (Val (v1), Val (v2)) in
     let reduct = match v1 with
       | Fun (x, e_fun) -> subst e_fun (x, v2)  (* e_fun[v2/x] *)
       | _ -> failwith "type error" in
     memo redex reduct ctxt;
     let result = f reduct ctxt in
     result)
