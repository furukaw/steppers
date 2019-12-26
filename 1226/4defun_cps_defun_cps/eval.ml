open Syntax
open Util
open Memo

let rec eval (exp : e) (k : k) (k2 : k2) : a =
  match exp with
  | Val (v) -> apply_in k v k2
  | App (e1, e2) -> eval e2 (FApp2 (e1, k)) k2
  | Op (name, e) -> eval e (FOp (name, k)) k2
  | With (e1, e2) -> eval e1 (FWith (e2, k)) k2

and apply_handler (cont_last : k) (h : h) (a : a) (k2 : k2)
  : a = match a with
  | Return v ->
    begin match h.return with
      | None -> apply_in cont_last v k2
      | Some (x, e) ->
        let reduct = subst e [(x, v)] in
        eval reduct cont_last k2 end
  | OpCall (name, v, k') ->
    begin match search_op name h with
      | None ->
        apply_out k2 (OpCall (name, v, FCall (cont_last, h, k')))
      | Some (x, k, e) ->
        let new_var = gen_var_name () in
        let cont_value =
          Cont (new_var, fun cont_last -> FCall (cont_last, h, k')) in
        let reduct = subst e [(x, v); (k, cont_value)] in
        eval reduct cont_last k2
    end

and apply_in (k : k) (v : v) (k2 : k2) : a =
  match k with
  | FId -> apply_out k2 (Return v)
  | FApp2 (e1, k) -> let v2 = v in
    eval e1 (FApp1 (v2, k)) k2
  | FApp1 (v2, k) -> let v1 = v in
    begin match v1 with
      | Fun (x, e) ->
        let reduct = subst e [(x, v2)] in
        eval reduct k k2
      | Cont (x, k') ->
        apply_in (k' k) v2 k2
      | _ -> failwith "type error"
    end
  | FOp (name, k) -> apply_out k2 (OpCall (name, v, k))
  | FWith (e2, k) -> let v1 = v in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    eval e2 FId (GHandle (k, h, k2))
  | FCall (cont_last, h, k') ->
    apply_in k' v (GHandle (cont_last, h, k2))

and apply_out (k2 : k2) (a : a) : a = match k2 with
  | GId -> a
  | GHandle (k, h, k2) ->
    apply_handler k h a k2

let stepper (e : e) : a =
  eval e FId GId
