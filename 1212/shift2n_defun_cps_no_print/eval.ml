open Syntax
open Util
open Memo

let rec eval (exp : e) (cont_in : cont_in) : v =
  match exp with
  | Val (v) -> apply_in cont_in v
  | App (e1, e2) ->
    eval e2 (FApp2 (e1, cont_in))
  | Shift (x, e) ->
    let cont_value = Cont (cont_in) in
    let reduct = Reset (subst e [(x, cont_value)]) in
    eval reduct FId
  | Reset (e) ->
    let v = eval e FId in
    apply_in cont_in v

and apply_in (cont_in : cont_in) (v : v) : v = match cont_in with
  | FId -> v
  | FApp2 (e1, cont_in) -> let v2 = v in
    eval e1 (FApp1 (v2, cont_in)) 
  | FApp1 (v2, cont_in) -> let v1 = v in
    begin
        match v1 with
        | Fun (x, e) ->
          let reduct = subst e [(x, v2)] in
          eval reduct cont_in
        | Cont (cont_in') ->
          apply_in cont_in (apply_in cont_in' v2)
        | _ -> failwith "type error"
      end

let stepper (e : e) : v = eval e FId
