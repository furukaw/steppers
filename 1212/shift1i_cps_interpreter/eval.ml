open Syntax
open Util
open Memo

let id_in : cont_in = fun v -> v

let rec eval (exp : e) ((ctxt_in, ctxt_out) : ctxt) (cont_in : cont_in) : v =
  match exp with
  | Val (v) -> cont_in v
  | App (e1, e2) ->
    eval e2 (CApp2 (e1) :: ctxt_in, ctxt_out) (fun v2 ->
        eval e1 (CApp1 (v2) :: ctxt_in, ctxt_out) (fun v1 ->
            match v1 with
            | Fun (x, e) ->
              let reduct = subst e [(x, v2)] in
              eval reduct (ctxt_in, ctxt_out) cont_in
            | Cont (cont_in', x, ctxt_in') ->
              cont_in (cont_in' v2)
            | _ -> failwith "type error"
          )
      )
  | Shift (x, e) ->
    let (ctxt_in', ctxt_out') = match ctxt_out with
      | [] -> failwith "no reset around shift"
      | (CReset (ctxt_in')) :: ctxt_out' -> (ctxt_in', ctxt_out') in
    let new_var = gen_var_name () in
    let cont_value = Cont (cont_in, new_var, ctxt_in) in
    let reduct = Reset (subst e [(x, cont_value)]) in
    eval reduct (ctxt_in', ctxt_out') id_in
  | Reset (e) ->
    let v = eval e ([], CReset ctxt_in :: ctxt_out) (fun v ->
        v) in
    cont_in v

let stepper (e : e) : v = eval e ([], []) id_in
