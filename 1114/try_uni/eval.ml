open Syntax
open Util
open Memo

(* try5 CPSステッパを非関数化してCPS変換して非関数化してコンテキスト情報を削除 *)
let rec eval (e : e) (cont_in : cont_in) (cont_out : cont_out) : a =
  match e with
  | Val (v) -> apply_in cont_in v cont_out
  | Fun (x, e1) -> apply_in cont_in (VFun (x, e1)) cont_out
  | App (e1, e2) ->
    eval e2 (FApp2 (e1, cont_in, cont_out)) cont_out
  | Raise (e1) ->
    eval e1 (FRaise (cont_in, cont_out)) cont_out
  | Try (e1, x, e2) ->
    eval e1 FId (GTry (x, e2, cont_in, cont_out))

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out) : a =
  match cont_in with
  | FId -> apply_out cont_out (Value v)
  | FApp2 (e1, cont_in, cont_out) ->
    eval e1 (FApp1 (v, cont_in, cont_out)) cont_out
  | FApp1 (v2, cont_in, cont_out) ->
    let redex = App (Val v, Val v2) in
    let reduct = match v with
      | VFun (x, e_fun) -> subst e_fun [(x, v2)]
      | _ -> failwith "type error" in
    memo redex reduct (cont_in, cont_out);
    eval reduct cont_in cont_out
  | FRaise (cont_in, cont_out) ->
    if cont_in <> FId then begin
      let redex = plug_in_try (Raise (Val v)) cont_in in
      let reduct = Raise (Val v) in
      memo redex reduct (FId, cont_out)
    end;
    apply_out cont_out (Raised (v))

and apply_out (cont_out : cont_out) (a : a) : a = match cont_out with
  | GId -> a
  | GTry (x, e2, cont_in, cont_out) -> let a1 = a in
    match a1 with
    | Value v1 ->
      let redex = Try (Val v1, x, e2) in
      let reduct = Val v1 in
      memo redex reduct (cont_in, cont_out);
      apply_in cont_in v1 cont_out
    | Raised (v) ->
      let redex = Try (Raise (Val v), x, e2) in
      let reduct = subst e2 [(x, v)] in
      memo redex reduct (cont_in, cont_out);
      eval reduct  cont_in cont_out

let interpreter (e : e) : a =
  eval e FId GId
