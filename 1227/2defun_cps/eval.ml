open Syntax
open Util
open Memo

let rec eval (exp : e) (k : k) : a = match exp with
  | Val (v) -> apply_in k v
  | App (e1, e2) -> eval e2 (FApp2 (e1, k))
  | Op (name, e) -> eval e (FOp (name, k))
  | With (h, e) ->
    let a = eval e FId in
    apply_handler k h a

and apply_in (k : k) (v : v) : a = match k with
  | FId -> Return v
  | FApp2 (e1, k) -> let v2 = v in
    eval e1 (FApp1 (v2, k))
  | FApp1 (v2, k) -> let v1 = v in
    (match v1 with
     | Fun (x, e) ->
       let reduct = subst e [(x, v2)] in
       eval reduct k
     | Cont (x, k') ->
       apply_in (k' k) v2
     | _ -> failwith "type error"
    )
  | FOp (name, k) -> OpCall (name, v, k)
  | FCall (k_last, h, k') ->
    let a = apply_in k' v in
    apply_handler k_last h a

and apply_handler (k_last : k) (h : h) (a : a) : a = match a with
  | Return v ->
    (match h with {return = (x, e)} ->
       let reduct = subst e [(x, v)] in
       eval reduct k_last)
  | OpCall (name, v, k') ->
    (match search_op name h with
     | None ->
       OpCall (name, v, FCall (k_last, h, k'))
     | Some (x, k, e) ->
       let new_var = gen_var_name () in
       let cont_value =
         Cont (new_var, fun k_last -> FCall (k_last, h, k')) in
       let reduct = subst e [(x, v); (k, cont_value)] in
       eval reduct k_last)

let stepper (e : e) : a = eval e FId
