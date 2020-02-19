open Syntax
open Util
open Memo

let eval_binop (n1 : int) (binop : binop) (n2 : int) : int =
  let meta_op = match binop with
    | Plus -> (+) | Minus -> (-) | Times -> ( * ) in
  meta_op n1 n2

(* CPS インタプリタ *)
let rec eval (exp : e) (k : k) : a = match exp with
  | Val (v) -> k v  (* 継続に値を渡す *)
  | App (e1, e2) ->
    eval e2 (fun v2 ->
        eval e1 (fun v1 -> match v1 with
            | Fun (x, e) ->
              let reduct = subst e [(x, v2)] in
              eval reduct k
            | Cont (cont_value) -> (cont_value k) v2
            (* 現在の継続の後ろに継続値が保持するメタ継続を合成して値を渡す *)
            | _ -> failwith "type error"))
  | Op (name, e) ->
    eval e (fun v -> OpCall (name, v, k))  (* 現在の継続の情報を返す *)
  | With (h, e) ->
    begin
      let a = eval e (fun v -> Return v) in
      apply_handler k h a
    end
  | BinOp (e1, binop, e2) ->
    eval e2 (fun v2 ->
        eval e1 (fun v1 ->
            match (v1, v2) with
            | (Int (n1), Int (n2)) -> k (Int (eval_binop n1 binop n2))
            | _ -> failwith "type error"))

and apply_handler (k : k) (h : h) (a : a) : a = match a with
  | Return v ->
    (match h.return with
     | None -> k v
     | Some (x, e) ->
       let reduct = subst e [(x, v)] in
       eval reduct k)
  | OpCall (name, v, k') ->
    (match search_op name h with
     | None ->
       OpCall (name, v, (fun v ->
           let a' = k' v in
           apply_handler k h a'))
     | Some (x, y, e) ->
       let cont_value =
         Cont (fun k'' -> fun v ->
             let a' = k' v in
             apply_k_to_a k'' a') in
       let reduct = subst e [(x, v); (y, cont_value)] in
       eval reduct k)

and apply_k_to_a (k : k) (a : a) : a = match a with
  | Return v -> k v
  | OpCall (name, v, k') ->
    OpCall (name, v, (fun v ->
        let a' = k' v in
        apply_k_to_a k a'))

(* 初期継続を渡して実行を始める *)
let interpreter (e : e) : a = eval e (fun v -> Return v)
