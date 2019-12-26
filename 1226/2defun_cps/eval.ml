open Syntax
open Util
open Memo

let rec eval (exp : e) (k : k) : a =
  match exp with
  | Val (v) -> apply_in k v
  | App (e1, e2) -> eval e2 (FApp2 (e1, k))
  | Op (name, e) -> eval e (FOp (name, k))
  | With (e1, e2) -> eval e1 (FWith (e2, k))

and apply_handler (cont_last : k) (h : h) (a : a) : a =
  match a with
  | Return v ->
    begin match h.return with
      | None -> apply_in cont_last v
      | Some (x, e) ->
        let reduct = subst e [(x, v)] in
        eval reduct cont_last end
  | OpCall (name, v, k') ->
    begin match search_op name h with
      | None ->
        OpCall (name, v, FCall (cont_last, h, k'))
      | Some (x, k, e) ->
        let new_var = gen_var_name () in
        let cont_value =
          Cont (new_var, fun cont_last -> FCall (cont_last, h, k')) in
        let reduct = subst e [(x, v); (k, cont_value)] in
        eval reduct cont_last
    end

and apply_in (k : k) (v : v) : a =
  match k with
  | FId -> Return v
  | FApp2 (e1, k) -> let v2 = v in
    eval e1 (FApp1 (v2, k))
  | FApp1 (v2, k) -> let v1 = v in
    begin match v1 with
      | Fun (x, e) ->
        let reduct = subst e [(x, v2)] in
        eval reduct k
      | Cont (x, k') ->
        apply_in (k' k) v2
      | _ -> failwith "type error"
    end
  | FOp (name, k) -> OpCall (name, v, k)
  | FWith (e2, k) -> let v1 = v in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    let a = eval e2 FId in
    apply_handler k h a
  | FCall (cont_last, h, k') ->
    let a = apply_in k' v in
    apply_handler cont_last h a

let stepper (e : e) : a = eval e FId
