open Syntax
open Value
open Context
open Util

let id : v -> v = fun v -> v

let rec f (t : t) (env : Env.t) (ctxt : ctxt) (c : c) : v = match t with
  | Var (x) -> c (Env.get env x)
  | Fun (x, t) ->
    c (VFun ((fun v c' -> f t (Env.add env x v) ctxt c'), (x, t)))
  | App (t1, t2) ->
    f t2 env (add_frame (App2 (t1)) ctxt) (fun v2 ->
        f t1 env (add_frame (App1 (v2)) ctxt) (fun v1 ->
            match v1 with
            | VFun (f, _) -> f v2 c
            | VCont (c', _) -> c (c' v2)
          )
      )
  | Shift (x, t) ->
    let (ctxt_in_reset, resets) = ctxt in
    let new_var = gen_var_name () in
    let cont_term =
      (new_var, Reset (plug_frames (Var (new_var)) ctxt_in_reset)) in
    f t (Env.add env x (VCont (c, cont_term))) ([], resets) id
  | Reset (t) ->
    c (f t env (add_reset ctxt) id)

let eval (t : t) = f t Env.empty Context.empty id
    

