open Syntax
open Util
open Memo

let id : cont = fun v -> Value v

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) (cont : cont) : a =
  match e with
  | Val (v) -> cont v
  | Fun (x, e1) ->
    cont (VFun ((fun v ctxt' cont' ->
        let redex = App (Fun (x, e1), Val v) in
        let reduct = subst e1 [(x, v)] in
        memo redex reduct ctxt';
        eval reduct ctxt' cont'),
                (x, e1)))
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out) (fun v2 ->
        eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out) (fun v1 ->
            match v1 with
            | VFun (f, (x, e_fun)) -> f v2 (ctxt_in, ctxt_out) cont
            | _ -> failwith "type error"
          )
      )
  | Raise (e1) ->
    eval e1 (add_frame CRaise ctxt_in, ctxt_out) (fun v ->
        Raised (v, ctxt_in)
      )
  | Try (e1, x, e2) ->
    let a1 = eval e1 ([], add_try x e2 ctxt_in ctxt_out) id in
    match a1 with
    | Value v1 ->
      let redex = Try (Val v1, x, e2) in
      let reduct = Val v1 in
      memo redex reduct (ctxt_in, ctxt_out);
      cont v1
    | Raised (v, ctxt_around_raise) ->
      if ctxt_around_raise <> []
      then begin
        let redex1 = plug_in_try (Raise (Val v)) ctxt_around_raise in
        let reduct1 = Raise (Val v) in
        memo redex1 reduct1 ([], add_try x e2 ctxt_in ctxt_out)
      end;
      let redex2 = Try (Raise (Val v), x, e2) in
      let reduct2 = subst e2 [(x, v)] in
      memo redex2 reduct2 (ctxt_in, ctxt_out);
      eval reduct2 (ctxt_in, ctxt_out) cont

let interpreter (e : e) : a =
  let result = eval e ([], []) id in
  match result with
  | Value v -> result
  | Raised (v, ctxt) ->
    let redex = plug_in_try (Raise (Val v)) ctxt in
    let reduct = (Raise (Val v)) in
    memo redex reduct ([], []);
    result
