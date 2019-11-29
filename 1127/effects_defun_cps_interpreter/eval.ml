open Syntax
open Util
open Memo

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt) (cont : cont) : a =
  match e with
  | Val (v) -> apply cont v
  | App (e1, e2) ->
    eval e2 (add_frame (CApp2 (e1)) ctxt_in, ctxt_out)
      (FApp2 (e1, (ctxt_in, ctxt_out), cont))
  | Op (op, e1) ->
    eval e1 (add_frame (COp (op)) ctxt_in, ctxt_out) (FOp (op, ctxt_in, cont))
  | With (e1, e2) ->
    eval e1 (add_frame (CWith (e2)) ctxt_in, ctxt_out)
      (FWith (e2, (ctxt_in, ctxt_out), cont))

and apply (cont : cont) (v : v) : a = match cont with
  | FId -> Return v
  | FApp2 (e1, (ctxt_in, ctxt_out), cont) -> let v2 = v in
    eval e1 (add_frame (CApp1 (v2)) ctxt_in, ctxt_out)
      (FApp1 (v2, (ctxt_in, ctxt_out), cont))
  | FApp1 (v2, (ctxt_in, ctxt_out), cont) -> let v1 = v in
    begin
      match v1 with
      | Fun (x, e_fun) ->
        (* let redex = App (Val v1, Val v2) in *)
        let reduct = subst e_fun [(x, v2)] in
        (* memo redex reduct (ctxt_in, ctxt_out); *)
        eval reduct (ctxt_in, ctxt_out) cont
      | Cont (cont', (x, e_fun)) ->
        (* let redex = App (Val v1, Val v2) in
         * let reduct = subst e_fun [(x, v2)] in
         * memo redex reduct (ctxt_in, ctxt_out); *)
        apply (FCont (cont', cont)) v2
      | _ -> failwith "type error"
    end
  | FOp (op, ctxt_in, cont) -> let v1 = v in
    OpCall (op, v1, (ctxt_in, []), cont)
  | FWith (e2, (ctxt_in, ctxt_out), cont) -> let v1 = v in
    let {return; ops} as h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    let a2 =  eval e2 ([], add_handle h ctxt_in ctxt_out) FId in
    begin match a2 with
      | Return v2 ->
        (* let redex = With (Val (Handler h), Val v2) in *)
        let reduct = match return with
          | None -> Val v2
          | Some (x, e) -> subst e [(x, v2)] in
        (* memo redex reduct (ctxt_in, ctxt_out); *)
        eval reduct (ctxt_in, ctxt_out) cont
      | OpCall (op, v, (ctxt_in', ctxt_out'), cont') ->
        begin match search_op op h with
          | Some (x, k, e) ->
            (* let redex =
             *   With (Val (Handler h),
             *         plug_all (Op (op, Val v)) (ctxt_in', ctxt_out')) in *)
            let new_var = gen_var_name () in
            let k_v = Cont (cont', (new_var,
                                    With (Val v1, plug_all (Val (Var new_var))
                                            (ctxt_in', ctxt_out')))) in
            let reduct = subst e [(x, v); (k, k_v)] in
            (* memo redex reduct (ctxt_in, ctxt_out); *)
            eval reduct (ctxt_in, ctxt_out) cont
          | None ->
            OpCall (op, v, (ctxt_in', ctxt_out' @ [CHandle (h, ctxt_in)]),
                    FCont (cont', cont))
        end
    end
  | FCont (cont1, cont2) ->
    match apply cont1 v with
    | Return v' -> apply cont2 v'
    | OpCall (op, v', ctxt', cont') ->
      OpCall (op, v', ctxt', cont')

let interpreter (e : e) : a = eval e ([], []) FId

