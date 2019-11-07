open Syntax
open Util
open Context

type a_t = Value of v_t
         | OpCall of (string * v_t * c_t)

let id : v_t -> a_t = fun v -> Value v

let find target pairs =
  try Some (List.find (fun (name, x, k, e) -> name = target) pairs)
  with Not_found -> None

type cont = FId
          | FApp2 of e_t * c_t * cont  (* e1, ctxt, k *)
          | FApp1 of v_t * c_t * cont  (* v2, ctxt, k *)
          | FOp of string * c_t   (* name, ctxt *)
          | FWith of e_t * c_t * cont  (* e2, ctxt, k *)

let rec f (exp : e_t) (ctxt : c_t) (k : cont) : a_t = match exp with
  | Val (v) -> apply k v
  | App (e1, e2) ->
    f e2 (add_frame (CAppR (e1)) ctxt) (FApp2 (e1, ctxt, k))
  | Op (name, e) ->
    f e (add_frame (COp (name)) ctxt) (FOp (name, ctxt))
  | With (e1, e2) ->
    f e1 (add_frame (CWith (e2)) ctxt) (FWith (e2, ctxt, k))

and apply (k : cont) (v : v_t) : a_t = match k with
  | FId -> Value v
  | FApp2 (e1, ctxt, k) ->
    let v2 = v in
    f e1 (add_frame (CAppL (v2)) ctxt) (FApp1 (v2, ctxt, k))
  | FApp1 (v2, ctxt, k) ->
    let v1 = v in
    let redex = App (Val v1, Val v2) in
    let reduct = match v1 with
      | Fun (x, e) -> subst e [(x, v2)]
      | _ -> failwith "type error" in
    memo redex reduct ctxt;
    f reduct ctxt k
  | FOp (name, ctxt) ->
    OpCall (name, v, ctxt)
  | FWith (e2, ctxt, k) ->
    let v1 = v in
    let {return; ops} as h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    match f e2 (add_handle h ctxt) FId with
    | Value v2 ->
      let redex = With (Val v1, Val v2) in
      let reduct = match return with
        | None -> Val v2
        | Some (x, e) -> subst e [(x, v2)] in
      memo redex reduct ctxt;
      f reduct ctxt k
    | OpCall (op, v, ctxt_around_call) ->
      match find op ops with
      | Some (_, x, k_op, e_op) ->
        let f_opv = plug_in_handles op (Op (op, Val v)) ctxt_around_call in
        let redex = With (Val v1, f_opv) in
        let y = gen_var_name () in
        let f_y = plug_in_handles op (Val (Var y)) ctxt_around_call in
        let k_val = Fun (y, With (Val v1, f_y)) in
        let reduct = subst e_op [(x, v); (k_op, k_val)] in
        memo redex reduct ctxt;
        f reduct ctxt k
      | None -> OpCall (op, v, ctxt_around_call)

let eval (e : e_t) : a_t =
  f e Context.empty FId
