open Syntax
open Util
open Memo

let rec eval (exp : e) ((ctxt_in, ctxt_out) : ctxt) : v =
  match exp with
  | Val (v) -> v
  | App (e1, e2) ->
    begin
      let v2 = eval e2 (CApp2 (e1, ctxt_in), ctxt_out) in
      let v1 = eval e1 (CApp1 (v2, ctxt_in), ctxt_out) in
      match v1 with
      | Fun (x, e) ->
        let reduct = subst e [(x, v2)] in
        eval reduct (ctxt_in, ctxt_out)
      | Cont (x, ctxt_in') ->
        let reduct = plug_in_handle (Val v2) (ctxt_in' CId) in
        eval reduct (ctxt_in, ctxt_out)
      | _ -> failwith "type error"
    end
  | Op (name, e) ->
    let v = eval e (COp (name, ctxt_in), ctxt_out) in
    raise (Call (name, v, ctxt_in))
  | With (e1, e2) ->
    let v1 = eval e1 (CWith (e2, ctxt_in), ctxt_out) in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    let a2 =
      try Return (eval e2 (CId, DHandle (ctxt_in, h, ctxt_out)))
      with Call (name, v, ctxt_in') -> OpCall (name, v, ctxt_in') in
    apply_handler (ctxt_in, ctxt_out) h a2

and apply_handler ((ctxt_in, ctxt_out) : ctxt) (h : h) (a : a) : v =
  match a with
  | Return v ->
    begin match h.return with
      | None -> v
      | Some (x, e) ->
        let reduct = subst e [(x, v)] in
        eval reduct (ctxt_in, ctxt_out) end
  | OpCall (name, v, ctxt_in') ->
    begin match search_op name h with
      | None ->
        raise (Call (name, v, CCall (ctxt_in, h, ctxt_in')))
      | Some (x, k, e) ->
        let new_var = gen_var_name () in
        let cont_value =
          Cont (new_var,
                fun ctxt_outer -> CCall (ctxt_in, h, ctxt_in')) in
        let reduct = subst e [(x, v); (k, cont_value)] in
        eval reduct (CId, ctxt_out)
    end

let stepper (e : e) : a =
  try Return (eval e (CId, DId))
  with Call (name, v, ctxt_in) -> OpCall (name, v, ctxt_in)
