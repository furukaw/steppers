open Syntax
open Util
open Memo

let rec eval (exp : e) (cont_in : cont_in) (cont_out : cont_out) : a =
  match exp with
  | Val (v) -> apply_in cont_in v cont_out
  | App (e1, e2) -> eval e2 (FApp2 (e1, cont_in)) cont_out
  | Op (name, e) -> eval e (FOp (name, cont_in)) cont_out
  | With (e1, e2) -> eval e1 (FWith (e2, cont_in)) cont_out

and apply_handler (cont_last : cont_in) (h : h) (a : a) (cont_out : cont_out)
  : a = match a with
  | Return v ->
    begin match h.return with
      | None -> apply_in cont_last v cont_out
      | Some (x, e) ->
        let reduct = subst e [(x, v)] in
        eval reduct cont_last cont_out end
  | OpCall (name, v, cont_in') ->
    begin match search_op name h with
      | None ->
        apply_out cont_out (OpCall (name, v, FCall (cont_last, h, cont_in')))
      | Some (x, k, e) ->
        let new_var = gen_var_name () in
        let cont_value =
          Cont (new_var, fun cont_last -> FCall (cont_last, h, cont_in')) in
        let reduct = subst e [(x, v); (k, cont_value)] in
        eval reduct cont_last cont_out
    end

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out) : a =
  match cont_in with
  | FId -> apply_out cont_out (Return v)
  | FApp2 (e1, cont_in) -> let v2 = v in
    eval e1 (FApp1 (v2, cont_in)) cont_out
  | FApp1 (v2, cont_in) -> let v1 = v in
    begin match v1 with
      | Fun (x, e) ->
        let reduct = subst e [(x, v2)] in
        eval reduct cont_in cont_out
      | Cont (x, cont_in') ->
        apply_in (cont_in' cont_in) v2 cont_out
      | _ -> failwith "type error"
    end
  | FOp (name, cont_in) -> apply_out cont_out (OpCall (name, v, cont_in))
  | FWith (e2, cont_in) -> let v1 = v in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    eval e2 FId (GHandle (cont_in, h, cont_out))
  | FCall (cont_last, h, cont_in') ->
    apply_in cont_in' v (GHandle (cont_last, h, cont_out))

and apply_out (cont_out : cont_out) (a : a) : a = match cont_out with
  | GId -> a
  | GHandle (cont_in, h, cont_out) ->
    apply_handler cont_in h a cont_out

let stepper (e : e) : a =
  eval e FId GId
