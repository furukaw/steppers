open Syntax
open Util
open Memo

exception Raised of v

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) : v = match e with
  | Val (v) -> v
  | Fun (x, e1) -> VFun (x, e1)
  | App (e1, e2) ->
    begin
      let v2 = eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out) in
      let v1 = eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out) in
      let redex = App (Val (v1), Val (v2)) in
      let reduct = match v1 with
        | VFun (x, e_fun) -> subst e_fun [(x, v2)]
        | _ -> failwith "type error" in
      memo redex reduct (ctxt_in, ctxt_out);
      let result = eval reduct (ctxt_in, ctxt_out) in
      result
    end
  | Raise (e1) ->
    let v = eval e1 (add_frame CRaise ctxt_in, ctxt_out) in
    if ctxt_in <> []
    then begin
      let redex = plug_in_try (Raise (Val v)) ctxt_in in
      let reduct = Raise (Val v) in
      memo redex reduct ([], ctxt_out)
    end;
    raise (Raised (v))
  | Try (e1, x, e2) ->
    begin try
        let v1 = eval e1 ([], add_try x e2 ctxt_in ctxt_out) in
        let redex = Try (Val v1, x, e2) in
        let reduct = Val v1 in
        memo redex reduct (ctxt_in, ctxt_out);
        v1
      with Raised (v) ->
        let redex = Try (Raise (Val v), x, e2) in
        let reduct = subst e2 [(x, v)] in
        memo redex reduct (ctxt_in, ctxt_out);
        eval reduct (ctxt_in, ctxt_out)
    end

let stepper (e : e) : (v, v) result =
  try Ok (eval e ([], []))
  with Raised (v) ->
    Error (v)
