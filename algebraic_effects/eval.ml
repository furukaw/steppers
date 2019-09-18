open Syntax
open Util
open Context

(* 式とコンテキストの情報を受け取って評価して値を返す *)
(* 簡約ごとに簡約の内容を標準出力する *)
let rec f (com : c_t) (ctxt : ctx_t) : c_t = match com with
  | Do (x, c1, c2) ->
    begin
      let c1_result = f c1 (CDo (x, c2) :: ctxt) in
      let reduct = match c1_result with
        | Return (v) -> subst c2 x v
        | Op (name, v, y, c3) ->
          Op (name, v, y, Do (x, c3, c2))
        | _ -> failwith "not defined" in
      f reduct ctxt
    end
  | _ -> com
