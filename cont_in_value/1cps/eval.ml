open Syntax
open Util
open Context

exception TypeError of e_t * string

let find target ops =
  try Some (List.find (fun (name, x, k, e) -> name = target) ops)
  with Not_found -> None

let id : cont_t = fun v -> Value v

(* 簡約ステップを出力しながら式を実行する *)
let rec f (exp : e_t) (ctxt : ctx_t) (c : cont_t) : a_t = match exp with
  | Val (v) -> c v
  | App (e1, e2) ->
    f e2 (add_frame (CAppR (e1)) ctxt) (fun v2 ->
        f e1 (add_frame (CAppL (v2)) ctxt) (fun v1 ->
            let redex = App (Val v1, Val v2) in
            let reduct = match v1 with
              | Fun (x, e) -> subst e [(x, v2)]
              | Cont (var, cont1, ctx1) ->
                subst (plug (Val (Var var)) ctx1) [(var, v2)]
              | _ -> failwith "type error" in
            memo redex reduct ctxt;
            f reduct ctxt c
          )
      )
  | Op (name, e) ->
    f e (add_frame (COp (name)) ctxt) (fun v ->
        OpCall (name, v, ctxt, c)
      )
  | With (e1, e2) ->
    f e1 (add_frame (CWith (e2)) ctxt) (fun v1 ->
        let {return; ops} as h = match v1 with
          |Handler (h) -> h
          | _ -> raise (TypeError (e1, "not a handler")) in
        match f e2 (add_handle h ctxt) id with
        | Value v2 ->
          let redex = With (Val v1, Val v2) in
          let reduct = match return with
            | None -> Val v2
            | Some (x, e) -> subst e [(x, v2)] in
          memo redex reduct ctxt;
          f reduct ctxt c
        | OpCall (op, v, ctx_op, cont_op) ->
          match find op ops with
          | Some (_, x, k, e_op) ->
            let f_opv = plug_in_handles op (Op (op, Val v)) ctx_op in
            let redex = With (Val v1, f_opv) in
            let y = gen_var_name () in
            let f_y = plug_in_handles op (Val (Var y)) ctx_op in
            let k_val = Cont (y, (add_handle h ctxt)
                                    With (Val v1, f_y)) in
            let reduct = subst e_op [(x, v); (k, k_val)] in
            memo redex reduct ctxt;
            f reduct ctxt c
          | None -> OpCall (op, v, ctx_op, cont_op)
      )

let eval (e : e_t) : a_t =
  f e empty_context id
   
