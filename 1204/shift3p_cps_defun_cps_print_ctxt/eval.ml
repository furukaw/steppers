open Syntax
open Util
open Memo

let hole = Val (Var "8")

let rec plug_cont_in (e : e) (cont_in : cont_in) : e = match cont_in with
  | FId -> e
  | FApp2 (e1, _, cont_in) -> plug_cont_in (App (e1, e)) cont_in
  | FApp1 (v2, _, cont_in) -> plug_cont_in (App (e, (Val v2))) cont_in

let id_out : cont_out = fun v ->
  print_newline ();
  print_endline "  apply_out";
  print_string "    exp : "; print_v v;
  v
  
(* 簡約ステップを出力しながら式を実行する *)
let rec eval (e : e) ((ctxt_in, ctxt_out) : ctxt)
    (cont_in : cont_in) (cont_out : cont_out) : v =
  print_newline ();
  print_endline "  eval";
  print_string "    exp : "; print_e e;
  print_string "    oin : "; print_e (plug_cont_in hole cont_in);
  print_string "    xin : "; print_e (plug_in_reset hole ctxt_in);
  print_string "    xout: "; print_e (plug_all hole ([], ctxt_out));
  match e with
  | Val (v) -> apply_in cont_in v ctxt_out cont_out
  | App (e1, e2) ->
    eval e2 ((CApp2 e1) :: ctxt_in, ctxt_out)
      (FApp2 (e1, ctxt_in, cont_in)) cont_out
  | Shift (x, e1) ->
    begin match ctxt_out with
      | [] -> failwith "no reset around shift"
      | CReset (ctxt_in', cont_in') :: ctxt_out' ->
        let redex = Reset (plug_in_reset e ctxt_in) in
        let new_var = gen_var_name () in
        let x_cont_in = Cont (new_var, ctxt_in, cont_in) in
        let reduct = Reset (subst e1 [(x, x_cont_in)]) in
        memo redex reduct (ctxt_in', ctxt_out');
        eval reduct (ctxt_in', ctxt_out') cont_in' (fun v ->
            print_newline ();
            print_endline "  apply_out1";
            print_string "    exp : "; print_v v;
            apply_in FId v )
    end
  | Reset (e1) ->
    eval e1 ([], CReset (ctxt_in, cont_in) :: ctxt_out) FId (fun v ->
        print_newline ();
        print_endline "  apply_out2";
        print_string "    exp : "; print_v v;
        let redex = Reset (Val v) in
        let reduct = Val v in
        memo redex reduct (ctxt_in, ctxt_out);
        apply_in cont_in v ctxt_out cont_out
      )

and apply_in (cont_in : cont_in) (v : v) (ctxt_out : ctxt_out)
    (cont_out : cont_out): v =
  print_newline ();
  print_endline "  apply_in";
  print_string "    exp : "; print_v v;
  print_string "    oin : "; print_e (plug_cont_in hole cont_in);
  print_string "    xout: "; print_e (plug_all hole ([], ctxt_out));
  match cont_in with
  | FId ->
    cont_out v
  | FApp2 (e1, ctxt_in2, cont_in2) -> let v2 = v in
    eval e1 (CApp1 (v2) :: ctxt_in2, ctxt_out)
      (FApp1 (v2, ctxt_in2, cont_in2)) cont_out
  | FApp1 (v2, ctxt_in1, cont_in1) -> let v1 = v in
    begin
      match v1 with
      | Fun (x, e_fun) ->
        let redex = App (Val v1, Val v2) in
        let reduct = subst e_fun [(x, v2)] in
        memo redex reduct (ctxt_in1, ctxt_out);
        eval reduct (ctxt_in1, ctxt_out) cont_in1 cont_out
      | Cont (x, ctxt_in', cont_in') ->
        let redex = App (Val v1, Val v2) in
        let reduct = Reset (plug_in_reset (Val v2) ctxt_in') in
        memo redex reduct (ctxt_in1, ctxt_out);
        apply_in cont_in' v2 (CReset ([], FId) :: ctxt_out) (fun v ->
            print_newline ();
            print_endline "  apply_out3";
            print_string "    exp : "; print_v v;
            let redex = Reset (Val v) in
            let reduct = Val v in
            memo redex reduct (ctxt_in1, ctxt_out);
            apply_in FId v ctxt_out cont_out
          )
      | _ -> failwith "type error"
    end

let stepper (e : e) : v =
  eval e ([], []) FId id_out
