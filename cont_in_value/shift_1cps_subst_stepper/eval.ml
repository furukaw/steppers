open Syntax
open Context
open Util

let id : v -> v = fun v -> v

let rec f (term : t) (ctxt : ctxt) (c : cont) : v = match term with
  | Val (v) -> c v
  | App (t1, t2) ->
    f t2 (add_frame (App2 (t1)) ctxt) (fun v2 ->
        f t1 (add_frame (App1 (v2)) ctxt) (fun v1 ->
            match v1 with
            | Fun (x, t0) ->
              let redex = App (Val v1, Val v2) in
              let reduct = subst t0 [(x, v2)] in
              memo redex reduct ctxt;
              f reduct ctxt c
            | Cont (c', (x, t0)) ->
              let redex = App (Val v1, Val v2) in
              let reduct = subst t0 [(x, v2)] in
              memo redex reduct ctxt;
              c (c' v2)
            | _ -> failwith "type error"
          )
      )
  | Shift (x, t) ->
    let (ctxt_inside_reset, ctxt_outside_reset) = peel ctxt in
    let new_var = gen_var_name () in
    let cont_term =
      Cont (c, (new_var,
                Reset (plug_frames (Val (Var (new_var))) ctxt_inside_reset))) in
    let redex = Reset (plug_frames term ctxt_inside_reset) in
    let reduct = Reset (subst t [(x, cont_term)]) in
    memo redex reduct ctxt_outside_reset;
    f reduct ctxt_outside_reset id
  | Reset (t) ->
    let v = f t (add_reset ctxt) id in
    memo (Reset (Val v)) (Val v) ctxt;
    c v

let eval (t : t) = f t Context.empty id
    

