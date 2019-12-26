open Syntax
open Util
open Memo

let id_in : cont_in = fun v -> Return v

let rec eval (exp : e) (cont_in : cont_in) : a =
  match exp with
  | Val (v) -> cont_in v
  | App (e1, e2) ->
    eval e2 (fun v2 ->
        eval e1 (fun v1 ->
            match v1 with
            | Fun (x, e) ->
              let reduct = subst e [(x, v2)] in
              eval reduct cont_in
            | Cont (x, cont_in') ->
              (cont_in' cont_in) v2
            | _ -> failwith "type error"
          )
      )
  | Op (name, e) ->
    eval e (fun v -> OpCall (name, v, cont_in))
  | With (e1, e2) ->
    eval e1 (fun v1 ->
        let h = match v1 with
          | Handler (h) -> h
          | _ -> failwith "type error" in
        let a = eval e2 id_in in
        apply_handler cont_in h a
      )

and apply_handler (cont_last : cont_in) (h : h) (a : a) : a =
  match a with
  | Return v ->
    begin match h.return with
      | None -> cont_last v
      | Some (x, e) ->
        let reduct = subst e [(x, v)] in
        eval reduct cont_last end
  | OpCall (name, v, cont_in') ->
    begin match search_op name h with
      | None ->
        OpCall (name, v, (fun v ->
            let a' = cont_in' v in
            apply_handler cont_last h a'))
      | Some (x, k, e) ->
        let new_var = gen_var_name () in
        let cont_value =
          Cont (new_var,
                fun cont_last -> fun v ->
                  let a' = cont_in' v in
                  apply_handler cont_last h a') in
        let reduct = subst e [(x, v); (k, cont_value)] in
        eval reduct cont_last
    end

let stepper (e : e) : a = eval e id_in
