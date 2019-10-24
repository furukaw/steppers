open Syntax
open Util
open Context

exception Raised of v_t * c_t
exception TypeError of e_t * string

(* 簡約ステップを出力しながら式を実行する *)
let rec f (exp : e_t) (ctxt : c_t) : v_t = match exp with
  | Val (v) -> v
  | App (e1, e2) ->
    let v2 = f e2 (add_frame (CAppR (e1)) ctxt) in
    let v1 = f e1 (add_frame (CAppL (v2)) ctxt) in
    let redex = App (Val v1, Val v2) in
    let reduct = match v1 with
      | Fun (x, e) -> subst e (x, v2)
      | _ -> failwith "type error" in
    memo redex reduct ctxt;
    f reduct ctxt
  | Raise (e) ->
    let v = f e (add_frame CRaise ctxt) in
    raise (Raised (v, ctxt))
  | Try (e0, x, e1) -> begin
      try
        let v0 = f e0 (add_try x e1 ctxt) in
        let (redex, reduct) = (Try (Val v0, x, e1), Val v0) in
        memo redex reduct ctxt;
        v0
      with
      | Raised (v, ctxt_around_raise) ->
        let redex1 = plug_in_try (Raise (Val v)) ctxt_around_raise in
        let reduct1 = Raise (Val v) in
        if redex1 <> reduct1
        then memo redex1 reduct1 (add_try x e1 ctxt);
        let redex2 = Try (reduct1, x, e1) in
        let reduct2 = subst e1 (x, v) in
        memo redex2 reduct2 ctxt;
        f reduct2 ctxt
    end
    
let eval (e : e_t) : (v_t, v_t) result =
  try Ok (f e Context.empty)
  with
  | Raised (v, ctxt) ->
    let redex = plug (Raise (Val v)) ctxt in
    let reduct = Raise (Val v) in
    memo redex reduct Context.empty;
    Error v
