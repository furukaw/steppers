open Syntax
open Util
open Memo

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) (ctxt : ctxt) (cont : cont) : v = match e with
  | Val (v) -> apply cont v
  | Fun (x, e1) -> apply cont (VFun (x, e1))
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt) (FApp2 (e1, ctxt, cont))

and apply (cont : cont) (v : v) : v = match cont with
  | FId -> v
  | FApp2 (e1, ctxt, cont) -> let v2 = v in
    eval e1 (add_frame (CApp1 (v2)) ctxt) (FApp1 (v2, ctxt, cont))
  | FApp1 (v2, ctxt, cont) -> let v1 = v in
    match v1 with
    | VFun (x, e_fun) ->
      (* let redex = App (Val v1, Val v2) in *)
      let reduct = subst e_fun [(x, v2)] in
      (* memo redex reduct ctxt; *)
      eval reduct ctxt cont
    | _ -> failwith "type error"

let interpreter (e : e) : v = eval e [] FId
