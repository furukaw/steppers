open Syntax
open Util
open Memo

 (* op とハンドラを受け取って、ハンドラで op が定義されていればその情報を返す *)
let search_op (op : string) ({ops} : h) : (string * string * e) option =
  try
    let (_, x, k, e) = List.find (fun (name, x, k, e) -> name = op) ops in
    Some (x, k, e)
  with Not_found -> None

let rec eval (exp : e) (cont_in : cont_in) (cont_out : cont_out) : a =
  print_newline ();
  print_endline "  eval";
  print_string "    exp: "; print_e exp;
  print_string "    in : "; print_e (plug_in_handle hole cont_in);
  print_string "    out: "; print_e (plug_all hole (FId, cont_out));
  match exp with
  | Val (v) -> apply_in cont_in v cont_out
  | App (e1, e2) -> eval e2 (FApp2 (e1, cont_in)) cont_out
  | Op (name, e) -> eval e (FOp (name, cont_in)) cont_out
  | With (e1, e2) -> eval e1 (FWith (e2, cont_in)) cont_out

and apply_handler (cont_last : cont_in) (h : h) (a : a) (cont_out : cont_out)
  : a = match a with
  | Return v ->
    begin match h.return with
      | None ->
        let redex = With (Val (Handler h), Val v) in
        let reduct = Val v in
        memo redex reduct (cont_last, cont_out);
        apply_in cont_last v cont_out
      | Some (x, e) ->
        let redex = With (Val (Handler h), Val v) in
        let reduct = (subst e [(x, v)]) in
        memo redex reduct (cont_last, cont_out);
        eval reduct cont_last cont_out end
  | OpCall (name, v, cont_in') ->
    begin match search_op name h with
      | None ->
        apply_out cont_out (OpCall (name, v, FNone (cont_last, h, cont_in')))
      | Some (x, k, e) ->
        let redex = With (Val (Handler h), plug_in_handle (Op (name, Val v)) cont_in') in
        let new_var = gen_var_name () in
        let cont_value =
          Cont (new_var, fun cont_last -> FSome (cont_last, h, cont_in')) in
        let reduct = subst e [(x, v); (k, cont_value)] in
        memo redex reduct (cont_last, cont_out);
        eval reduct cont_last cont_out
    end

and apply_in (cont_in : cont_in) (v : v) (cont_out : cont_out) : a =
  print_newline ();
  print_endline "  apply_in";
  print_string "    exp: "; print_v v;
  print_string "    in : "; print_e (plug_in_handle hole cont_in);
  print_string "    out: "; print_e (plug_all hole (FId, cont_out));
  match cont_in with
  | FId -> apply_out cont_out (Return v)
  | FApp2 (e1, cont_in) -> let v2 = v in
    eval e1 (FApp1 (v2, cont_in)) cont_out
  | FApp1 (v2, cont_in) -> let v1 = v in
    begin match v1 with
      | Fun (x, e) ->
        let redex = App (Val v1, Val v2) in
        let reduct = subst e [(x, v2)] in
        memo redex reduct (cont_in, cont_out);
        eval reduct cont_in cont_out
      | Cont (x, cont_in') ->
        let redex = App (Val v1, Val v2) in
        let reduct = plug_in_handle (Val v2) (cont_in' FId) in
        memo redex reduct (cont_in, cont_out);
        apply_in (cont_in' cont_in) v2 cont_out
      | _ -> failwith "type error"
    end
  | FOp (name, cont_in) -> apply_out cont_out (OpCall (name, v, cont_in))
  | FWith (e2, cont_in) -> let v1 = v in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    eval e2 FId (GHandle (cont_in, h, cont_out))
  | FNone (cont_last, h, cont_in') ->
    apply_in cont_in' v (GHandle (cont_last, h, cont_out))
  | FSome (cont_last, h, cont_in') ->
    apply_in cont_in' v (GHandle (cont_last, h, cont_out))

and apply_out (cont_out : cont_out) (a : a) : a =
  print_newline ();
  print_endline "  apply_out";
  print_string "    exp: "; print_a a;
  print_string "    in : "; print_e hole;
  print_string "    out: "; print_e (plug_all hole (FId, cont_out));
  match cont_out with
  | GId -> a
  | GHandle (cont_in, h, cont_out) ->
    apply_handler cont_in h a cont_out

let stepper (e : e) : a =
  eval e FId GId
