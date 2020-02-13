open Syntax
open Util
open Memo

(* CPS インタプリタを非関数化して CPS 変換したインタプリタ *)
let rec eval (exp : e) (k : k) (k2 : k2) : a = match exp with
  | Val (v) -> apply_in k v k2
  | App (e1, e2) -> eval e2 (FApp2 (e1, k)) k2
  | Op (name, e) -> eval e (FOp (name, k)) k2
  | With (h, e) -> eval e FId (fun a -> apply_handler k h a k2)

and apply_in (k : k) (v : v) (k2 : k2) : a = match k with
  | FId -> k2 (Return v)
  | FApp2 (e1, k) -> let v2 = v in
    eval e1 (FApp1 (v2, k)) k2
  | FApp1 (v2, k) -> let v1 = v in
    (match v1 with
     | Fun (x, e) ->
       let reduct = subst e [(x, v2)] in
       eval reduct k k2
     | Cont (k') ->
       apply_in (k' k) v2 k2
     | _ -> failwith "type error")
  | FOp (name, k) -> k2 (OpCall (name, v, k))
  | FCall (k_last, h, k') ->
    apply_in k' v (fun a -> apply_handler k_last h a k2)

and apply_handler (k_last : k) (h : h) (a : a) (k2 : k2) : a = match a with
  | Return v ->
    (match h with {return = (x, e)} ->
       let reduct = subst e [(x, v)] in
       eval reduct k_last k2)
  | OpCall (name, v, k') ->
    (match search_op name h with
     | None ->
       k2 (OpCall (name, v, FCall (k_last, h, k')))
     | Some (x, k, e) ->
      let cont_value =
         Cont (fun k_last -> FCall (k_last, h, k')) in
       let reduct = subst e [(x, v); (k, cont_value)] in
       eval reduct k_last k2)

let stepper (e : e) : a = eval e FId (fun a -> a)
