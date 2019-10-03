open Syntax
open Util
open Context

(* raise 式を実行する時に起こす例外 *)
exception Raised of (v_t * c_t)  (* (例外の値, 当時のコンテキスト) *)

(* 型エラーを表す例外 *)
exception TypeError of string * e_t

(* ステップを出力しながら式を実行する *)
let rec f (e : e_t) (ctxt : c_t) : v_t = match e with
  | Val (v) -> v
  | App (e1, e2) ->
    let v1 = f e1 (add_app1 e2 ctxt) in
    let v2 = f e2 (add_app2 v1 ctxt) in
    let redex = App (Val v1, Val v2) in
    let reduct = match v1 with
      | Fun (x, e_fun) -> subst e_fun (x, v2)
      | _ -> raise (TypeError ("app", redex)) in
    memo redex reduct ctxt;
    let result = f reduct ctxt in
    result
  | If (e0, e1, e2) ->
    let v0 = f e0 (add_if e1 e2 ctxt) in
    let redex = If (Val v0, e1, e2) in
    let reduct = match v0 with
      | True -> e1
      | False -> e2
      | _ -> raise (TypeError ("if", redex)) in
    memo redex reduct ctxt;
    let result = f reduct ctxt in
    result
  | Raise (e0) ->
    let v0 = f e0 (add_raise ctxt) in
    raise (Raised (v0, ctxt))
  | Try (e0, x, e1) -> begin
      try
        let v0 = f e0 (add_try x e1 ctxt) in
        let redex = Try (Val v0, x, e1) in
        let reduct = Val v0 in
        memo redex reduct ctxt;
        v0
      with Raised (v, ctxt_raised) -> begin
          if exists_frames_in_try ctxt_raised
          then begin
            let redex1 = plug_in_try (Raise (Val v)) ctxt_raised in
            let reduct1 = Raise (Val v) in
            memo redex1 reduct1 (add_try x e1 ctxt)
          end;
          let redex2 = Try (Raise (Val v), x, e1) in
          let reduct2 = subst e1 (x, v) in
          memo redex2 reduct2 ctxt;
          let result = f reduct2 ctxt in
          result
        end
    end

(* 空のコンテキストで式の実行を開始する *)
let stepper (e : e_t) : a_t =
  try Ok (f e Context.empty)
  with
  | Raised (v, ctxt) ->
    if exists_frames_in_try ctxt
    then
      memo (plug_in_try (Raise (Val v)) ctxt) (Raise (Val v)) Context.empty;
    Error (v)
