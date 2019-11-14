open Syntax
open Context
open Util

let id : v -> v = fun v -> v

let rec eval (term : t) (ctxt : ctxt) (c : cont) : v = match term with
  | Val (v) -> v
  | Fun (x, t) ->
    c (VFun ((fun v c' -> eval (subst t [(x, v)]) ctxt c'), (x, t)))
  | App (t1, t2) ->
    eval t2 (add_frame (App2 (t1)) ctxt) (fun v2 ->
        eval t1 (add_frame (App1 (v2)) ctxt) (fun v1 ->
            match v1 with
            | VFun (f, (x, t0)) -> f v2 c
            | VCont (c', _) -> c (c' v2)
            | _ -> failwith "type error"
          )
      )
  | Shift (x, t) ->
    let (ctxt_in_reset, resets) = ctxt in
    let new_var = gen_var_name () in
    let cont_term =
      (new_var, Reset (plug_frames (Val (Var (new_var))) ctxt_in_reset)) in
    eval (subst t [(x, (VCont (c, cont_term)))]) ([], resets) id
  | Reset (t) ->
    c (eval t (add_reset ctxt) id)

let eval (t : t) = eval t Context.empty id
    

