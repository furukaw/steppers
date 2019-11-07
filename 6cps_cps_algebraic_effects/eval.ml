open Syntax
open Util
open Context

type a_t = Value of v_t
         | OpCall of string * v_t * c_t

type cc_t = v_t -> (a_t -> a_t) -> a_t
type cd_t = a_t -> a_t

let cid : cc_t = fun v d -> d (Value v)
let did : cd_t = fun a -> a

let find target pairs =
  try Some (List.find (fun (name, x, k, e) -> name = target) pairs)
  with Not_found -> None

let rec f (exp : e_t) (ctxt : c_t) (c : cc_t) (d : cd_t) =
  match exp with
  | Val (v) -> c v d
  | App (e1, e2) ->
    f e2 (add_frame (CAppR (e1)) ctxt) (fun v2 d2 ->
        f e1 (add_frame (CAppL (v2)) ctxt) (fun v1 d1 ->
            let redex = App (Val v1, Val v2) in
            let reduct = match v1 with
              | Fun (x, e) -> subst e [(x, v2)]
              | _ -> failwith "type error" in
            memo redex reduct ctxt;
            f reduct ctxt c d1
          ) d2
      ) d
  | Op (name, e) ->
    f e (add_frame (COp (name)) ctxt) (fun v d' ->
        d' (OpCall (name, v, ctxt))) d
  | With (e1, e2) ->
    f e1 (add_frame (CWith (e2)) ctxt) (fun v1 d1 ->
        let {return; ops} as h = match v1 with
          | Handler (h) -> h
          | _ -> failwith "type error" in
        f e2 (add_handle h ctxt) cid (fun a2 ->
            match a2 with
            | Value v2 ->
              let redex = With (Val v1, Val v2) in
              let reduct = match return with
                | None -> Val v2
                | Some (x, e) -> subst e [(x, v2)] in
              memo redex reduct ctxt;
              f reduct ctxt c d1
            | OpCall (op, v, ctxt_around_call) ->
              match find op ops with
              | Some (_, x, k_op, e_op) ->
                let f_opv =
                  plug_in_handles op (Op (op, Val v)) ctxt_around_call in
                let redex = With (Val v1, f_opv) in
                let y = gen_var_name () in
                let f_y = plug_in_handles op (Val (Var y)) ctxt_around_call in
                let k_val = Fun (y, With (Val v1, f_y)) in
                let reduct = subst e_op [(x, v); (k_op, k_val)] in
                memo redex reduct ctxt;
                f reduct ctxt c d1
              | None -> d1 a2
          )
      ) d

let eval (e : e_t) : a_t =
  f e Context.empty cid did
