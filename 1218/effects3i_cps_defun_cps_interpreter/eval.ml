open Syntax
open Util
open Memo

(* op とハンドラを受け取って、ハンドラで op が定義されていればその情報を返す *)
let search_op (op : string) ({ops} : h) : (string * string * e) option =
  try
    let (_, x, k, e) = List.find (fun (name, x, k, e) -> name = op) ops in
    Some (x, k, e)
  with Not_found -> None

(* inner の外側（後）に outer をつけた継続 *)
let rec compose_cont_in (inner : cont_in) (outer : cont_in) : cont_in =
  match inner with
  | FId -> outer
  | FApp2 (e1, cont_in) -> FApp2 (e1, compose_cont_in cont_in outer)
  | FApp1 (v2, cont_in) -> FApp1 (v2, compose_cont_in cont_in outer)
  | FOp (name, cont_in) -> FOp (name, compose_cont_in cont_in outer)
  | FWith (e2, cont_in) -> FWith (e2, compose_cont_in cont_in outer)

(* inner の外側（後）に (with h handle cont_in) をつけた継続 *)
(* let rec wrap_cont_out (inner: cont_out) (h : h) (cont_in : cont_in) : cont_out =
 *   match inner with
 *   | GId -> GHandle (h, (cont_in, GId))
 *   | GHandle (h', (cont_in', cont_out')) ->
 *     GHandle (h', (cont_in', wrap_cont_out cont_out' h cont_in)) *)

(* 継続の合成 *)
(* let rec compose_cont (inner_in, inner_out) (outer_in, outer_out) : cont =
 *   match inner_out with
 *   | GId -> (compose_cont_in inner_in outer_in, outer_out)
 *   | GHandle (h, (cont_in', cont_out')) ->
 *     (inner_in,
 *      GHandle (h, compose_cont (cont_in', cont_out') (outer_in, outer_out))) *)

let compose_cont (inner_in, inner_out) (outer_in, outer_out) : cont =
  match inner_out with
  

(* インタプリタ *)
let rec eval (exp : e) (cont_in : cont_in) (cont_out : cont_out) : a =
  match exp with
  | Val (v) -> apply_in cont_in v cont_out
  | App (e1, e2) ->
    eval e2 (FApp2 (e1, cont_in)) cont_out
  | Op (name, e) ->
    eval e (FOp (name, cont_in)) cont_out
  | With (e1, e2) ->
    eval e1 (FWith (e2, cont_in)) cont_out

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out) : a =
  match cont_in with
  | FId -> apply_out cont_out (Return v)
  | FApp2 (e1, cont_in) -> let v2 = v in
    eval e1 (FApp1 (v2, cont_in)) cont_out
  | FApp1 (v2, cont_in) -> let v1 = v in
    begin  match v1 with
      | Fun (x, e) ->
        let reduct = subst e [(x, v2)] in
        eval reduct cont_in cont_out
      | Cont (x, (cont_in', cont_out')) ->
        let (new_cont_in, new_cont_out) =
          compose_cont (cont_in', cont_out') (cont_in, cont_out) in
        apply_in new_cont_in v2 new_cont_out
      | _ -> failwith "type error"
    end
  | FOp (name, cont_in) ->
    apply_out cont_out (OpCall (name, v, (cont_in, GId)))
  | FWith (e2, cont_in) -> let v1 = v in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    eval e2 FId (GHandle (h, (cont_in, cont_out)))

 and apply_out (cont_out : cont_out) (a : a) =
  match cont_out with
  | GId -> a
  | GHandle (h, (cont_in', cont_out')) ->
    match a with
    | Return v ->
      begin match h.return with
        | None -> apply_in cont_in' v cont_out'
        | Some (x, e) ->
          let reduct = subst e [(x, v)] in
          eval reduct cont_in' cont_out'
      end
    | OpCall (name, v, (cont_in'', cont_out'')) ->
      match search_op name h with
      | Some (x, k, e) ->
        let new_var = gen_var_name () in
        let reduct =
          subst e [(x, v);
                   (k, Cont (new_var, (cont_in'',
                                       wrap_cont_out cont_out'' h FId)))] in
        eval reduct cont_in' cont_out'
      | None -> apply_out
                  cont_out'
                  (OpCall (name, v, (cont_in'',
                                     wrap_cont_out cont_out'' h cont_in')))

(* インタプリタの入り口 *)
let stepper (e : e) : a =
  eval e FId GId
