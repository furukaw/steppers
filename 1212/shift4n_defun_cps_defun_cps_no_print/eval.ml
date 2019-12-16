open Syntax
open Util
open Memo

let rec eval (exp : e) (cont_in : cont_in) (cont_out : cont_out) : v =
  match exp with
  | Val (v) -> apply_in cont_in v cont_out
  | App (e1, e2) ->
    eval e2 (FApp2 (e1, cont_in)) cont_out
  | Shift (x, e) ->
    let cont_value = Cont (cont_in) in
    let reduct = Reset (subst e [(x, cont_value)]) in
    eval reduct FId cont_out
  | Reset (e) ->
    eval e FId (GReset (cont_in, cont_out))

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out) : v =
  match cont_in with
  | FId -> apply_out cont_out v
  | FApp2 (e1, cont_in) -> let v2 = v in
    eval e1 (FApp1 (v2, cont_in)) cont_out
  | FApp1 (v2, cont_in) -> let v1 = v in
    begin
        match v1 with
        | Fun (x, e) ->
          let reduct = subst e [(x, v2)] in
          eval reduct cont_in cont_out
        | Cont (cont_in') ->
          apply_in cont_in' v2 (GReset (cont_in, cont_out))
        | _ -> failwith "type error"
      end

and apply_out (cont_out : cont_out) (v : v) : v = match cont_out with
  | GId -> v
  | GReset (cont_in, cont_out) ->
    apply_in cont_in v cont_out

let stepper (e : e) : v = eval e FId GId
