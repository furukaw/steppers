open Syntax
open Util
open Context

type a_t = Value of v_t
         | OpCall of string * v_t * c_t

type cc_t = v_t -> (a_t -> a_t) -> a_t
type cd_t = a_t -> a_t

let cid : cc_t = fun v d -> d (Value v)
let did : cd_t = fun a -> a

type c_cont = CId
            | FAppR of e_t * c_t * c_cont  (* e1, ctxt, c *)
            | FAppL of v_t * c_t * c_cont  (* v2, ctxt, c *)
            | FOp of string * c_t          (* name, ctxt *)
            | FWith of e_t * c_t * c_cont  (* e2, ctxt, c *)

and d_cont = DId
           | FHandle of v_t * h_t * c_t * c_cont * d_cont
(* v1, h, ctxt, c, d1 *)

let find target pairs =
  try Some (List.find (fun (name, x, k, e) -> name = target) pairs)
  with Not_found -> None

let rec f (exp : e_t) (ctxt : c_t) (c : c_cont) (d : d_cont) : a_t =
  match exp with
  | Val (v) -> apply_c c v d
  | App (e1, e2) ->
    f e2 (add_frame (CAppR (e1)) ctxt) (FAppR (e1, ctxt, c)) d
  | Op (name, e) ->
    f e (add_frame (COp (name)) ctxt) (FOp (name, ctxt)) d
  | With (e1, e2) ->
    f e1 (add_frame (CWith (e2)) ctxt) (FWith (e2, ctxt, c)) d

and apply_c (c : c_cont) (v : v_t) (d : d_cont) : a_t = match c with
  | CId -> apply_d d (Value v)
  | FAppR (e1, ctxt, c) ->
    let (v2, d2) = (v, d) in
    f e1 (add_frame (CAppL (v2)) ctxt) (FAppL (v2, ctxt, c)) d2
  | FAppL (v2, ctxt, c) ->
    let (v1, d1) = (v, d) in
    (* let redex = App (Val v1, Val v2) in *)
    let reduct = match v1 with
      | Fun (x, e) -> subst e [(x, v2)]
      | _ -> failwith "type error" in
    (* memo redex reduct ctxt; *)
    f reduct ctxt c d1
  | FOp (name, ctxt) ->
    let d' = d in
    apply_d d' (OpCall (name, v, ctxt))
  | FWith (e2, ctxt, c) ->
    let (v1, d1) = (v, d) in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    f e2 (add_handle h ctxt) CId (FHandle (v1, h, ctxt, c, d1))

and apply_d (d : d_cont) (a : a_t) : a_t = match d with
  | DId -> a
  | FHandle (v1, h, ctxt, c, d1) ->
    let a2 = a in
    match a2 with
    | Value v2 ->
      (* let redex = With (Val v1, Val v2) in *)
      let reduct = match h.return with
        | None -> Val v2
        | Some (x, e) -> subst e [(x, v2)] in
      (* memo redex reduct ctxt; *)
      f reduct ctxt c d1
    | OpCall (op, v, ctxt_around_call) ->
      match find op h.ops with
      | Some (_, x, k_op, e_op) ->
        (* let f_opv =
         *   plug_in_handles op (Op (op, Val v)) ctxt_around_call in
         * let redex = With (Val v1, f_opv) in *)
        let y = gen_var_name () in
        let f_y = plug_in_handles op (Val (Var y)) ctxt_around_call in
        let k_val = Fun (y, With (Val v1, f_y)) in
        let reduct = subst e_op [(x, v); (k_op, k_val)] in
        (* memo redex reduct ctxt; *)
        f reduct ctxt c d1
      | None -> apply_d d1 a2


let eval (e : e_t) : a_t =
  f e Context.empty CId DId
