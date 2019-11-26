open Syntax
open Util
open Memo

let id : cont = fun v -> Value v

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) (cont : cont) : a =
  match e with
  | Val (v) -> cont v
  | Fun (x, e1) -> cont (VFun (x, e1))
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out) (fun v2 ->
        eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out) (fun v1 ->
            let redex = App (Val v1, Val v2) in
            let reduct = match v1 with
              | VFun (x, e_fun) -> subst e_fun [(x, v2)]
              | _ -> failwith "type error" in
            memo redex reduct (ctxt_in, ctxt_out);
            eval reduct (ctxt_in, ctxt_out) cont
          )
      )
  | Raise (e1) ->
    eval e1 (add_frame CRaise ctxt_in, ctxt_out) (fun v ->
        if ctxt_in <> []
        then begin
          let redex = plug_in_try (Raise (Val v)) ctxt_in in
          let reduct = Raise (Val v) in
          memo redex reduct ([], ctxt_out)
        end;
        Raised (v)
      )
  | Try (e1, x, e2) ->
    let a1 = eval e1 ([], add_try x e2 ctxt_in ctxt_out) id in
    match a1 with
    | Value v1 ->
      let redex = Try (Val v1, x, e2) in
      let reduct = Val v1 in
      memo redex reduct (ctxt_in, ctxt_out);
      cont v1
    | Raised (v) ->
      let redex = Try (Raise (Val v), x, e2) in
      let reduct = subst e2 [(x, v)] in
      memo redex reduct (ctxt_in, ctxt_out);
      eval reduct (ctxt_in, ctxt_out) cont

let interpreter (e : e) : a =
  eval e ([], []) id
