open Syntax
open Util
open Context
open Io

let do_op2 (op : defined_fun_t) (arg1 : v_t) (arg2 : v_t) : v_t =
  match (op, arg1, arg2) with
  | (Join, String (s1), String (s2)) ->
    if s1 = "" || s2 = "" then String (s1 ^ s2) else String (s1 ^ " " ^ s2)
  | _ -> failwith "type error in op2"

(* join のせいで生まれた関数 *)
let rec eval_value (value : v_t) (ctxt : cvalue_t) : v_t =
  match value with
  | Pair (v1, v2) ->
    let v2' = eval_value v2 (CPair2 (v1, ctxt)) in
    let v1' = eval_value v1 (CPair1 (v2', ctxt)) in
    Pair (v1', v2')
  | Op2 (op, v1, v2) ->
    let v2' = eval_value v2 (CPair2 (v1, ctxt)) in
    let v1' = eval_value v1 (CPair1 (v2', ctxt)) in
    let redex = Op2 (op, v1', v2') in
    let reduct = do_op2 op v1' v2' in
    memo_for_value redex reduct ctxt;
    reduct
  | _ -> value

(* 式とコンテキストの情報を受け取って評価して値を返す *)
(* 簡約ごとに簡約の内容を標準出力する *)
let rec f (com : c_t) (ctxt : ctx_t) : c_t = match com with
  | Do (p, c1, c2) ->
    let c1_result = f c1 (CDo (p, c2) :: ctxt) in
    let reduct = match c1_result with
      | Return (v) -> subst_pattern c2 p v
      | Op (op, v, y, c3) ->
        Op (op, v, y, Do (p, c3, c2))
      | _ -> failwith ("type error: " ^ c_to_string com) in
    memo (Do (p, c1_result, c2)) reduct ctxt;
    f reduct ctxt
  | Seq (c1, c2) ->
    let c1_result = f c1 (CSeq (c2) :: ctxt) in
    let reduct = match c1_result with
      | Return (v) -> c2
      | Op (op, v, y, c3) ->
        Op (op, v, y, Seq (c3, c2))
      | _ -> failwith ("type error: " ^ c_to_string com) in
    memo (Seq (c1_result, c2)) reduct ctxt;
    f reduct ctxt
  | If (True, c1, c2) ->
    memo com c1 ctxt;
    f c1 ctxt
  | If (False, c1, c2) ->
    memo com c2 ctxt;
    f c2 ctxt
  | If (_, _, _) -> failwith ("type error: " ^ c_to_string com)
  | App (Fun (x, c), v) ->
    let v' = match v with
      | Op2 (op, v1, v2) ->
        let reduct_op = do_op2 op v1 v2 in
        memo (App (Fun (x, c), v)) (App (Fun (x, c), reduct_op)) ctxt;
        reduct_op
      | _ -> v in
    let redex = App (Fun (x, c), v') in
    let reduct = subst c x v' in
    memo redex reduct ctxt;
    f reduct ctxt
  | App (_, _) -> failwith ("type error: " ^ c_to_string com)
  | With (Handler (h), c) ->
    let c_result = f c (CWith (Handler h) :: ctxt) in
    let reduct = match (c_result, h) with
      | (Return (v), (Some (x, c_r), _)) -> subst c_r x v
      | (Return (_), (None, _)) -> c_result
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
  | With (_, _) -> failwith ("type error: " ^ c_to_string com)
  | Op (op, v, y, c) ->
    begin match ctxt with
      | CDo (_, _) :: rest -> com
      | CSeq (_) :: rest -> com
      | CWith (_) :: rest -> com
      | _ ->
        let (op_result, input, output) = do_op op v in
        let reduct = subst c y op_result in
        memo com reduct ~input:input ~output:output ctxt;
        f reduct ctxt
    end
  | Return (Op2 (op, v1, v2)) ->
    let reduct = Return (do_op2 op v1 v2) in
    memo com reduct ctxt;
    reduct
  | Return (v) -> Return (eval_value v (CReturn ctxt))

