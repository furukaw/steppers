open Syntax
open Util
open Context

exception Call of string * v_t * c_t
exception TypeError of e_t * string

(* 簡約ステップを出力しながら式を実行する *)
let rec f (exp : e_t) (ctxt : c_t) : v_t = match exp with
  | Val (v) -> v
  | App (e1, e2) ->
    let v2 = f e2 (add_frame (CAppR (e1)) ctxt) in
    let v1 = f e1 (add_frame (CAppL (v2)) ctxt) in
    let redex = App (Val v1, Val v2) in
    let reduct = match v1 with
      | Fun (x, e) -> subst e [(x, v2)]
      | _ -> failwith "type error" in
    memo redex reduct ctxt;
    f reduct ctxt
  | Op (name, e) ->
    let v = f e (add_frame (COp (name)) ctxt) in
    raise (Call (name, v, ctxt))
  | With (e1, e2) ->
    let v1 = f e1 (add_frame (CWith (e2)) ctxt) in
    let {return; ops} as h = match v1 with
      |Handler (h) -> h
      | _ -> raise (TypeError (e1, "not a handler")) in
    begin try
        let v2 = f e2 (add_handle h ctxt) in
        let redex = With (Val v1, Val v2) in
        let reduct = match return with
          | None -> Val v2
          | Some (x, e) -> subst e [(x, v2)] in
        memo redex reduct ctxt;
        f reduct ctxt
      with Call (op, v, ctxt_around_op) -> begin
          try
            let (_, x, k, e_op) =
              List.find (fun (name, x, k, e) -> name = op) ops in
            let f_opv = plug_in_handles op (Op (op, Val v)) ctxt_around_op in
            let redex = With (Val v1, f_opv) in
            let y = gen_var_name () in
            let f_y = plug_in_handles op (Val (Var y)) ctxt_around_op in
            let k_val = Fun (y, With (Val v1, f_y)) in
            let reduct = subst e_op [(x, v); (k, k_val)] in
            memo redex reduct ctxt;
            f reduct ctxt
          with Not_found -> raise (Call (op, v, ctxt_around_op))
        end end
    
let eval (e : e_t) : (v_t, e_t) result =
  try Ok (f e Context.empty)
  with
  | Call (op, v, ctxt) ->
    Error (plug (Op (op, Val v)) ctxt)
