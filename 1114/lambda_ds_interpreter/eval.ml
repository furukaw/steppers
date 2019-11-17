open Syntax
open Util

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) (ctxt : ctxt) : v = match e with
  | Val (v) -> v
  | Fun (x, e1) -> VFun (x, e1)
  | App (e1, e2) ->
    begin
      let v2 = eval e2 (add_frame (CApp2 (e1)) ctxt) in
      let v1 = eval e1 (add_frame (CApp1 (v2)) ctxt) in
      (* let redex = App (Val (v1), Val (v2)) in *)
      let reduct = match v1 with
        | VFun (x, e_fun) -> subst e_fun [(x, v2)]
        | _ -> failwith "type error" in
      (* memo redex reduct ctxt; *)
      let result = eval reduct ctxt in
      result
    end

let interpreter (e : e) : v = eval e []
    
