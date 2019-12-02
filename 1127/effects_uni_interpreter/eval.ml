open Syntax
open Util
open Memo

(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e)  (cont_in : cont_in) (cont_out : cont_out) : a =
  match e with
  | Val (v) -> apply cont_in v cont_out
  | App (e1, e2) ->
    eval e2 (FApp2 (e1, cont_in, cont_out)) cont_out
  | Op (op, e1) ->
    eval e1 (FOp (op, cont_in, cont_out)) cont_out
  | With (e1, e2) ->
    eval e1 (FWith (e2, cont_in, cont_out)) cont_out

and apply (cont_in : cont_in) (v : v) (cont_out : cont_out) : a =
  match cont_in with
  | FId -> apply_out cont_out (Return v)
  | FApp2 (e1, cont_in, cont_out) -> let v2 = v in
    eval e1 (FApp1 (v2, cont_in, cont_out)) cont_out
  | FApp1 (v2, cont_in, cont_out) -> let v1 = v in
    begin
      match v1 with
      | Fun (x, e_fun) ->
        (* let redex = App (Val v1, Val v2) in *)
        let reduct = subst e_fun [(x, v2)] in
        (* memo redex reduct (ctxt_in, ctxt_out); *)
        eval reduct cont_in cont_out
      | Cont (cont_in', (x, e_fun)) ->
        (* let redex = App (Val v1, Val v2) in
         * let reduct = subst e_fun [(x, v2)] in
         * memo redex reduct (ctxt_in, ctxt_out); *)
        apply (FCont (cont_in', cont_in)) v2 cont_out
      | _ -> failwith "type error"
    end
  | FOp (op, cont_in, cont_out) -> let v1 = v in
    apply_out cont_out (OpCall (op, v1, cont_in, cont_out, GId))
  | FWith (e2, cont_in, cont_out) -> let v1 = v in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    eval e2 FId (GHandle (h, cont_in, cont_out))
  | FCont (cont_in1, cont_in2) ->
    match apply cont_in1 v GId with
    | Return v' -> apply cont_in2 v' cont_out
    | OpCall (op, v', cont_in', cont_out', cont_out'') ->
      apply_out cont_out (OpCall (op, v', cont_in', cont_out', cont_out''))

and apply_out (cont_out : cont_out) (a : a) : a = match cont_out with
  | GId -> a
  | GHandle (h, cont_in, cont_out) -> let a2 = a in
    begin match a2 with
      | Return v2 ->
        (* let redex = With (Val (Handler h), Val v2) in *)
        let reduct = match h.return with
          | None -> Val v2
          | Some (x, e) -> subst e [(x, v2)] in
        (* memo redex reduct (ctxt_in, ctxt_out); *)
        eval reduct cont_in cont_out
      | OpCall (op, v, cont_in', cont_out', cont_out'') ->
        begin match search_op op h with
          | Some (x, k, e) ->
            (* let redex =
             *   With (Val (Handler h),
             *         plug_all (Op (op, Val v)) (ctxt_in', ctxt_out')) in *)
            let new_var = gen_var_name () in
            let k_v = Cont (cont_in', (new_var,
                                       With (Val (Handler h),
                                             plug_all (Val (Var new_var))
                                               (cont_in', cont_out'')))) in
            let reduct = subst e [(x, v); (k, k_v)] in
            (* memo redex reduct (ctxt_in, ctxt_out); *)
            eval reduct cont_in cont_out
          | None ->
            let new_cont_out = add_handle_out cont_out'' h cont_in cont_out in
            apply_out
              cont_out
              (OpCall
                 (op, v, cont_in', cont_out', new_cont_out))
        end
    end    

let interpreter (e : e) : a = eval e FId GId
