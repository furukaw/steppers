open Syntax
open Util
open Memo

(* CPS インタプリタを非関数化したインタプリタ *)
let rec eval (exp : e) (k : k) : a = match exp with
  | Val (v) -> apply_in k v  (* 値に継続を適用する *)
  | App (e1, e2) -> eval e2 (FApp2 (e1, k))
  | Op (name, e) -> eval e (FOp (name, k))
  | With (h, e) ->
    let a = eval e FId in
    apply_handler k h a

(* 継続を適用する関数 *)
and apply_in (k : k) (v : v) : a = match k with
  | FId -> Return v  (* 継続が空なのでその値を返す *)
  | FApp2 (e1, k) -> let v2 = v in
    eval e1 (FApp1 (v2, k))
  | FApp1 (v2, k) -> let v1 = v in
    (match v1 with
     | Fun (x, e) ->
       let reduct = subst e [(x, v2)] in
       eval reduct k
     | Cont (k') ->
       apply_in (k' k) v2  (* 現在の継続と継続値が保持するメタ継続を合成して値を渡す *)
     | _ -> failwith "type error")
  | FOp (name, k) -> OpCall (name, v, k)  (* 現在の継続の情報を返す *)
  | FCall (k'', h, k') ->
    let a = apply_in k' v in              (* v に k' を適用してから、その結果を *)
    apply_handler k'' h a                 (* h で処理したものに k'' を適用する *)

(* ハンドラを処理する関数 *)
and apply_handler (k : k) (h : h) (a : a) : a = match a with
  | Return v ->  (* handle 節内が値 v を返したとき *)
    (match h with {return = (x, e)} ->
       let reduct = subst e [(x, v)] in
       eval reduct k)
  | OpCall (name, v, k') ->
    (match search_op name h with
     | None ->                             (* ハンドラで定義されていなければ *)
       OpCall (name, v, FCall (k, h, k'))  (* OpCall の継続に現在の継続を合成 *)
     | Some (x, y, e) ->                   (* ハンドラで定義されていれば *)
       let cont_value =                    (* 適用時にその後の継続を受け取って合成 *)
         Cont (fun k -> FCall (k, h, k')) in
       let reduct = subst e [(x, v); (y, cont_value)] in
       eval reduct k)

let stepper (e : e) : a = eval e FId
