open Syntax
open Util
open Memo

 (* op とハンドラを受け取って、ハンドラで op が定義されていればその情報を返す *)
let search_op (op : string) ({ops} : h) : (string * string * e) option =
  try
    let (_, x, k, e) = List.find (fun (name, x, k, e) -> name = op) ops in
    Some (x, k, e)
  with Not_found -> None

let rec eval (exp : e) (cont_in : cont_in) : a =
  match exp with
  | Val (v) -> apply_in cont_in v
  | App (e1, e2) -> eval e2 (FApp2 (e1, cont_in))
  | Op (name, e) -> eval e (FOp (name, cont_in))
  | With (e1, e2) -> eval e1 (FWith (e2, cont_in))

and cont_with (cont_last : cont_in) (h : h) (a : a) : a =
  match a with
  | Return v ->
    begin match h.return with
      | None -> apply_in cont_last v
      | Some (x, e) -> eval (subst e [(x, v)]) cont_last end
  | OpCall (name, v, cont_in') ->
    begin match search_op name h with
      | None ->
        OpCall (name, v, FNone (cont_last, h, cont_in'))
      | Some (x, k, e) ->
        let new_var = gen_var_name () in
        let cont_value =
          Cont (new_var, fun id_in -> FSome (h, cont_in')) in
        let reduct = subst e [(x, v); (k, cont_value)] in
        eval reduct cont_last
    end

and apply_in (cont_in : cont_in) (v : v) : a =
  match cont_in with
  | FId -> Return v
  | FApp2 (e1, cont_in) -> let v2 = v in
    eval e1 (FApp1 (v2, cont_in))
  | FApp1 (v2, cont_in) -> let v1 = v in
    begin match v1 with
      | Fun (x, e) ->
        let reduct = subst e [(x, v2)] in
        eval reduct cont_in
      | Cont (x, cont_in') ->
        apply_in (cont_in' cont_in) v2
      | _ -> failwith "type error"
    end
  | FOp (name, cont_in) -> OpCall (name, v, cont_in)
  | FWith (e2, cont_in) -> let v1 = v in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    let a = eval e2 FId in
    cont_with cont_in h a
  | FNone (cont_last, h, cont_in') ->
    cont_with cont_last h (apply_in cont_in' v)
  | FSome (h, cont_in') ->
    cont_with FId h (apply_in cont_in' v)

let stepper (e : e) : v =
  let a = eval e FId in
  match a with
  | Return v -> v
  | OpCall (name, _, _) -> failwith ("no handlers for " ^ name)
