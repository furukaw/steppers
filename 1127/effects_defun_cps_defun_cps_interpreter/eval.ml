open Syntax
open Util
open Memo

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) (cont_in : cont_in) (cont_out : cont_out) : a =
  match e with
  | Val (v) -> apply cont_in v cont_out
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out)
      (FApp2 (e1, (ctxt_in, ctxt_out), cont_in)) cont_out
  | Op (op, e1) ->
    eval e1 (add_frame (COp (op)) ctxt_in, ctxt_out)
      (FOp (op, ctxt_in, cont_in)) cont_out
  | With (e1, e2) ->
    eval e1 (add_frame (CWith (e2)) ctxt_in, ctxt_out)
      (FWith (e2, (ctxt_in, ctxt_out), cont_in)) cont_out

and apply (cont_in : cont_in) (v : v) (cont_out : cont_out) : a =
  match cont_in with
  | FId -> apply_out cont_out (Return v)
  | FApp2 (e1, (ctxt_in, ctxt_out), cont_in) -> let v2 = v in
    eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out)
      (FApp1 (v2, (ctxt_in, ctxt_out), cont_in)) cont_out
  | FApp1 (v2, (ctxt_in, ctxt_out), cont_in) -> let v1 = v in
    begin
      match v1 with
      | Fun (x, e_fun) ->
        (* let redex = App (Val v1, Val v2) in *)
        let reduct = subst e_fun [(x, v2)] in
        (* memo redex reduct (ctxt_in, ctxt_out); *)
        eval reduct (ctxt_in, ctxt_out) cont_in cont_out
      | Cont (cont_in', (x, e_fun)) ->
        (* let redex = App (Val v1, Val v2) in
         * let reduct = subst e_fun [(x, v2)] in
         * memo redex reduct (ctxt_in, ctxt_out); *)
        apply (FCont (cont_in', cont_in)) v2 cont_out
      | _ -> failwith "type error"
    end
  | FOp (op, ctxt_in, cont_in) -> let v1 = v in
    apply_out cont_out (OpCall (op, v1, (ctxt_in, []), cont_in))
  | FWith (e2, (ctxt_in, ctxt_out), cont_in) -> let v1 = v in
    let {return; ops} as h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    eval e2 ([], add_handle h ctxt_in ctxt_out) FId
      (GHandle (h, (ctxt_in, ctxt_out), cont_in, cont_out))
  | FCont (cont_in1, cont_in2) ->
    match apply cont_in1 v GId with
    | Return v' -> apply cont_in2 v' cont_out
    | OpCall (op, v', ctxt', cont_in') ->
      apply_out cont_out (OpCall (op, v', ctxt', cont_in'))

and apply_out (cont_out : cont_out) (a : a) : a = match cont_out with
  | GId -> a
  | GHandle (h, (ctxt_in, ctxt_out), cont_in, cont_out) -> let a2 = a in
    begin match a2 with
      | Return v2 ->
        (* let redex = With (Val (Handler h), Val v2) in *)
        let reduct = match h.return with
          | None -> Val v2
          | Some (x, e) -> subst e [(x, v2)] in
        (* memo redex reduct (ctxt_in, ctxt_out); *)
        eval reduct (ctxt_in, ctxt_out) cont_in cont_out
      | OpCall (op, v, (ctxt_in', ctxt_out'), cont_in') ->
        begin match search_op op h with
          | Some (x, k, e) ->
            (* let redex =
             *   With (Val (Handler h),
             *         plug_all (Op (op, Val v)) (ctxt_in', ctxt_out')) in *)
            let new_var = gen_var_name () in
            let k_v = Cont (cont_in', (new_var,
                                       With (Val (Handler h), plug_all (Val (Var new_var))
                                               (ctxt_in', ctxt_out')))) in
            let reduct = subst e [(x, v); (k, k_v)] in
            (* memo redex reduct (ctxt_in, ctxt_out); *)
            eval reduct (ctxt_in, ctxt_out) cont_in cont_out
          | None ->
            apply_out cont_out (OpCall (op, v, (ctxt_in',
                                      ctxt_out' @ [CHandle (h, ctxt_in)]),
                              FCont (cont_in', cont_in)))
        end
    end    

let interpreter (e : e) : a = eval e ([], []) FId GId
