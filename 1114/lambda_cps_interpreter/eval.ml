open Syntax
open Util
open Memo

let id : cont = fun v -> v

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) (ctxt : ctxt) (cont : cont) : v = match e with
  | Val (v) -> cont v
  | Fun (x, e1) ->
    cont (VFun ((fun v ctxt' cont' ->
        (* let redex = App (Fun (x, e1), Val v) in *)
        let reduct = subst e1 [(x, v)] in
        (* memo redex reduct ctxt'; *)
        eval reduct ctxt' cont'),
                (x, e1)))
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt) (fun v2 ->
        eval e1 (add_frame (CApp1 (v2)) ctxt) (fun v1 ->
            match v1 with
            | VFun (f, (x, e_fun)) -> f v2 ctxt cont
            | _ -> failwith "type error"
          )
      )

let interpreter (e : e) : v = eval e [] id
