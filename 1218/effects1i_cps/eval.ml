open Syntax
open Util
open Memo

let id_in : cont_in = fun v -> Return v

(* op とハンドラを受け取って、ハンドラで op が定義されていればその情報を返す *)
let search_op (op : string) ({ops} : h) : (string * string * e) option =
  try
    let (_, x, k, e) = List.find (fun (name, x, k, e) -> name = op) ops in
    Some (x, k, e)
  with Not_found -> None

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
            | Cont (cont_in') ->
              begin
                match cont_in' v2 with
                | Return v -> cont_in v
                | OpCall (name, v, cont_ins) ->
                  OpCall (name, v, cont_ins)
              end
            | _ -> failwith "type error"
          )
      )
  | Op (name, e) ->
    eval e (fun v -> OpCall (name, v, [cont_in]))
  | With (e1, e2) ->
    eval e1 (fun v1 ->
        let h = match v1 with
          | Handler (h) -> h
          | _ -> failwith "type error" in
        match eval e2 id_in with
        | Return v2 ->
          begin match h.return with
            | None -> cont_in v2
            | Some (x, e) -> eval (subst e [(x, v2)]) cont_in end
        | OpCall (name, v2, cont_ins) ->
          begin match search_op name h with
            | None -> OpCall (name, v2, cont_in :: cont_ins)
            | Some (x, k, e) ->
              eval (subst e [(x, v2); (k, Cont (cont_in))]) id_in
          end
      )

let stepper (e : e) : v =
  match eval e id_in with
  | Return v -> v
  | OpCall (name, _, _) -> failwith ("no handlers for " ^ name)
