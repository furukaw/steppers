open Syntax
open Util
open Memo

let rec eval
    (e : e)
    ((ctxt_in, ctxt_out) : ctxt)
    (cont_in : cont_in) (cont_out : cont_out) : a =
  match e with
  | Val (v) -> apply_in cont_in v cont_out
  | Fun (x, e1) -> apply_in cont_in (VFun (x, e1)) cont_out
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out)
      (FApp2 (e1, ctxt_in, ctxt_out, cont_in)) cont_out
  | Raise (e1) ->
    eval e1 (add_frame CRaise ctxt_in, ctxt_out) (FRaise (ctxt_in, ctxt_out))
      cont_out
  | Try (e1, x, e2) ->
    eval e1 ([], add_try x e2 ctxt_in ctxt_out) FId
      (GTry (x, e2, ctxt_in, ctxt_out, cont_in, cont_out))

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out) : a =
  match cont_in with
  | FId -> apply_out cont_out (Value v)
  | FApp2 (e1, ctxt_in, ctxt_out, cont_in) ->
    eval e1 (add_frame (CApp1 (v)) ctxt_in, ctxt_out)
      (FApp1 (v, ctxt_in, ctxt_out, cont_in)) cont_out
  | FApp1 (v2, ctxt_in, ctxt_out, cont_in) ->
    let redex = App (Val v, Val v2) in
    let reduct = match v with
      | VFun (x, e_fun) -> subst e_fun [(x, v2)]
      | _ -> failwith "type error" in
    memo redex reduct (ctxt_in, ctxt_out);
    eval reduct (ctxt_in, ctxt_out) cont_in cont_out
  | FRaise (ctxt_in, ctxt_out) ->
    if ctxt_in <> [] then begin
      let redex = plug_in_try (Raise (Val v)) ctxt_in in
      let reduct = Raise (Val v) in
      memo redex reduct ([], ctxt_out)
    end;
    apply_out cont_out (Raised (v))

and apply_out (cont_out : cont_out) (a : a) : a = match cont_out with
  | GId -> a
  | GTry (x, e2, ctxt_in, ctxt_out, cont_in, cont_out) -> let a1 = a in
    match a1 with
    | Value v1 ->
      let redex = Try (Val v1, x, e2) in
      let reduct = Val v1 in
      memo redex reduct (ctxt_in, ctxt_out);
      apply_in cont_in v1 cont_out
    | Raised (v) ->
      let redex = Try (Raise (Val v), x, e2) in
      let reduct = subst e2 [(x, v)] in
      memo redex reduct (ctxt_in, ctxt_out);
      eval reduct (ctxt_in, ctxt_out) cont_in cont_out

let interpreter (e : e) : a =
  eval e ([], []) FId GId
