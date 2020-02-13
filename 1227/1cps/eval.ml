open Syntax
open Util
open Memo

(* インタプリタ *)
let rec eval (exp : e) (k : k) : a = match exp with
  | Val (v) -> k v
  | App (e1, e2) ->
    eval e2 (fun v2 ->
        eval e1 (fun v1 -> match v1 with
            | Fun (x, e) ->
              let reduct = subst e [(x, v2)] in
              eval reduct k
            | Cont (x, k') ->
              (k' k) v2
            | _ -> failwith "type error"))
  | Op (name, e) ->
    eval e (fun v -> OpCall (name, v, k))
  | With (h, e) ->
    let a = eval e (fun v -> Return v) in
    apply_handler k h a

(* ハンドラを処理する関数 *)
and apply_handler (k_last : k) (h : h) (a : a) : a = match a with
  | Return v ->
    (match h with {return = (x, e)} ->
       let reduct = subst e [(x, v)] in
       eval reduct k_last)
  | OpCall (name, v, k') ->
    (match search_op name h with
     | None ->
       OpCall (name, v, (fun v ->
           let a' = k' v in
           apply_handler k_last h a'))
     | Some (x, k, e) ->
       let new_var = gen_var_name () in
       let cont_value =
         Cont (new_var,
               fun k_last -> fun v ->
                 let a' = k' v in
                 apply_handler k_last h a') in
       let reduct = subst e [(x, v); (k, cont_value)] in
       eval reduct k_last)

(* 初期継続を渡して実行を始める *)
let interpreter (e : e) : a = eval e (fun v -> Return v)
