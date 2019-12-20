open Syntax
open Util
open Memo

(* op とハンドラを受け取って、ハンドラで op が定義されていればその情報を返す *)
let search_op (op : string) ({ops} : h) : (string * string * e) option =
  try
    let (_, x, k, e) = List.find (fun (name, x, k, e) -> name = op) ops in
    Some (x, k, e)
  with Not_found -> None

let rec eval (exp : e) (cont_in : cont_in) (cont_out : cont_out) : a =
  match exp with
  | Val (v) -> apply_in cont_in v cont_out
  | App (e1, e2) ->
    eval e2 (fun v2 cont_out ->
        eval e1 (fun v1 cont_out ->
            match v1 with
            | Fun (x, e) ->
              let reduct = subst e [(x, v2)] in
              eval reduct cont_in cont_out
            | Cont (cont_in', cont_out') ->
              apply_in cont_in' v2 (fun a -> cont_out (fun a -> cont_out' a))
      ) cont_out

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out) : a =
  match cont_in with
  | FId -> apply_out cont_out (Return v)

and apply_out (cont_out : cont_out) (a : a) : a = match cont_out with
  | GId -> a

let stepper (e : e) : v =
  match eval e id_in id_out with
  | Return v -> v
  | OpCall (name, _, _) -> failwith ("no handlers for " ^ name)
