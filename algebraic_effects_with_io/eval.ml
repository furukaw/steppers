open Syntax
open Util
open Context

(* 式とコンテキストの情報を受け取って評価して値を返す *)
(* 簡約ごとに簡約の内容を標準出力する *)
let rec f (com : c_t) (ctxt : ctx_t) : c_t = match com with
  | Do (p, c1, c2) ->
    let c1_result = f c1 (CDo (p, c2) :: ctxt) in
    let reduct = match c1_result with
      | Return (v) -> subst_pattern c2 p v
      | Op (name, v, y, c3) ->
        Op (name, v, y, Do (p, c3, c2))
      | _ -> failwith ("type error: " ^ c_to_string com) in
    memo (Do (p, c1_result, c2)) reduct ctxt;
    f reduct ctxt
  | If (True, c1, c2) ->
    memo com c1 ctxt;
    f c1 ctxt
  | If (False, c1, c2) ->
    memo com c2 ctxt;
    f c2 ctxt
  | If (_, _, _) -> failwith "type error"
  | App (Fun (x, c), v) ->
    let reduct = subst c x v in
    memo com reduct ctxt;
    f reduct ctxt
  | App (_, _) -> failwith "type error"
  | With (Handler (h), c) ->
    let c_result = f c (CWith (Handler h) :: ctxt) in
    let reduct = match (c_result, h) with
      | (Return (v), (Some (x, c_r), _)) -> subst c_r x v
      | (Return (_), (None, _)) -> failwith "not defined"
      | (Op (name, v, y, c), (_, op_lst)) -> begin
          try
            let (_, x, k, c_n) =
              List.find (fun (n, _, _, _) -> n = name) op_lst in
            subst_all c_n [(x, v); (k, Fun (y, With (Handler (h), c)))]
          with Not_found -> Op (name, v, y, With (Handler (h), c))
        end
      | _ -> failwith "type error" in
    memo (With (Handler (h), c_result)) reduct ctxt;
    f reduct ctxt
  | With (_, _) -> failwith "type error"
  | _ -> com
