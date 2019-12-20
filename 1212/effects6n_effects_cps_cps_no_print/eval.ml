open Syntax
open Util
open Memo

let id_in : cont_in = fun v cont_out -> cont_out (Return v)
let id_out : cont_out = fun a -> a

(* op とハンドラを受け取って、ハンドラで op が定義されていればその情報を返す *)
let search_op (op : string) ({ops} : h) : (string * string * e) option =
  try
    let (_, x, k, e) = List.find (fun (name, x, k, e) -> name = op) ops in
    Some (x, k, e)
  with Not_found -> None

let rec eval (exp : e) (cont_in : cont_in) (cont_out : cont_out) : a =
  match exp with
  | Val (v) -> cont_in v cont_out
  | App (e1, e2) ->
    eval e2 (fun v2 cont_out ->
        eval e1 (fun v1 cont_out ->
            match v1 with
            | Fun (x, e) ->
              let reduct = subst e [(x, v2)] in
              eval reduct cont_in cont_out
            | Cont (cont_in') ->
              begin
                match cont_in' v2 id_out with
                | Return v -> cont_in v cont_out
                | OpCall (name, v, cont_ins) ->
                  cont_out (OpCall (name, v, cont_ins))
              end
            | _ -> failwith "type error"
          ) cont_out
      ) cont_out
  | Op (name, e) ->
    eval e (fun v cont_out -> cont_out (OpCall (name, v, [cont_in]))) cont_out
  | With (e1, e2) ->
    eval e1 (fun v1 cont_out ->
        let h = match v1 with
          | Handler (h) -> h
          | _ -> failwith "type error" in
        eval e2 id_in (fun a2 -> match a2 with
            | Return v2 ->
              begin match h.return with
                | None -> cont_in v2 cont_out
                | Some (x, e) -> eval (subst e [(x, v2)]) cont_in cont_out end
            | OpCall (name, v2, cont_ins) ->
              begin match search_op name h with
                | None -> OpCall (name, v2, cont_in :: cont_ins)
                | Some (x, k, e) ->
                  eval (subst e [(x, v2); (k, Cont (cont_in))]) id_in cont_out
              end
          )
      ) cont_out

let stepper (e : e) : v =
  match eval e id_in id_out with
  | Return v -> v
  | OpCall (name, _, _) -> failwith ("no handlers for " ^ name)
