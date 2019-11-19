open Syntax
open Util
open Memo

let id : cont = fun v -> v

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) (ctxt : ctxt) (cont : cont) : v = match e with
  | Val (v) -> cont v
  | Fun (x, e1) ->
    cont (VFun (x, e1))
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt) (fun v2 ->
        eval e1 (add_frame (CApp1 (v2)) ctxt) (fun v1 ->
            (* let redex = App (Val v1, Val v2) in *)
            let reduct =  match v1 with
              | VFun (x, e_fun) -> subst e_fun [(x, v2)]
              | _ -> failwith "type error" in
            (* memo redex reduct ctxt; *)
            eval reduct ctxt cont
          )
      )
      
let interpreter (e : e) : v = eval e [] id
