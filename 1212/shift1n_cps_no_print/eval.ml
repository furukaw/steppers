open Syntax
open Util
open Memo

let id_in : cont_in = fun v -> v

let rec eval (exp : e) (cont_in : cont_in) : v =
  match exp with
  | Val (v) -> cont_in v
  | App (e1, e2) ->
    eval e2 (fun v2 ->
        eval e1 (fun v1 ->
            match v1 with
            | Fun (x, e) ->
              let reduct = subst e [(x, v2)] in
              eval reduct cont_in
            | Cont (cont_in') ->
              cont_in (cont_in' v2)
            | _ -> failwith "type error"
          )
      )
  | Shift (x, e) ->
    let cont_value = Cont (cont_in) in
    let reduct = Reset (subst e [(x, cont_value)]) in
    eval reduct id_in
  | Reset (e) ->
    let v = eval e id_in in
    cont_in v

let stepper (e : e) : v = eval e id_in
