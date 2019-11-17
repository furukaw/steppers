open Syntax
open Memo
open Util

let id : v -> v = fun v -> v

let rec eval (term : t) ((ctxt_in, ctxt_out) : ctxt) (c : cont) : v =
  match term with
  | Val (v) -> c v
  | Fun (x, t) ->
    c (VFun ((fun v ctxt' c' ->
        let t' = subst t [(x, v)] in
        memo (App (Fun (x, t), Val v)) t' ctxt';
        eval t' ctxt' c'), (x, t)))
  | App (t1, t2) ->
    eval t2 (add_frame (App2 (t1)) ctxt_in, ctxt_out) (fun v2 ->
        eval t1 (add_frame (App1 (v2)) ctxt_in, ctxt_out) (fun v1 ->
            match v1 with
            | VFun (f, (x, t0)) ->
              f v2 (ctxt_in, ctxt_out) c
            | VCont (c', (x, t0)) -> c (c' v2)
            | _ -> failwith "type error"
          )
      )
  | Shift (x, t) ->
    begin match ctxt_out with
      | [] -> failwith "no reset around shift"
      | first :: rest ->
        let redex = Reset (plug_frames term ctxt_in) in
        let new_var = gen_var_name () in
        let cont_term =
          (new_var, Reset (plug_frames (Val (Var (new_var))) ctxt_in)) in
        let reduct = Reset (subst t [(x, (VCont (c, cont_term)))]) in
        memo redex reduct (first, rest);
        eval reduct (first, rest) id
    end
  | Reset (t) ->
    let v = eval t ([], add_reset ctxt_in ctxt_out) id in
    memo (Reset (Val v)) (Val v) (ctxt_in, ctxt_out);
    c v

let eval (t : t) = eval t empty_context id
    

