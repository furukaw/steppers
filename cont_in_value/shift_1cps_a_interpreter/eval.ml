open Syntax
open Context
open Util

type a = Value of v
       | Shifting of string * t * ctxt

type cont = v -> a

let id : v -> a = fun v -> Value v

let rec eval (term : t) (ctxt : ctxt) (c : cont) : a = match term with
  | Val (v) -> c v
  | App (t1, t2) ->
    eval t2 (add_frame (App2 (t1)) ctxt) (fun v2 ->
        eval t1 (add_frame (App1 (v2)) ctxt) (fun v1 ->
            let reduct = match v1 with
              | Fun (x, t0) -> subst t0 [(x, v2)]
              | Cont (x, t0) -> subst t0 [(x, v2)]
              | _ -> failwith "type error" in
            eval reduct ctxt c
          )
      )
  | Shift (x, t) ->
    Shifting (x, t, ctxt)
  | Reset (t) ->
    let a = eval t (add_reset ctxt) id in
    match a with
    | Value (v) -> c v
    | Shifting (x, t0, ctxt_around_shift) ->
      let new_var = gen_var_name () in
      let cont_term =
        Fun (new_var,
             Reset (plug_in_reset (Val (Var (new_var))) ctxt_around_shift)) in
      eval (Reset (subst t0 [(x, cont_term)])) ctxt c
 
                                  
let eval (t : t) = eval t Context.empty id
    

