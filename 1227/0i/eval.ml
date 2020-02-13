open Syntax
open Util
open Memo

let hole = Val (Var "8")

type t = E of e | V of v | A of a

let print_t : t -> unit = function
  | E e -> print_e e;
  | V v -> print_v v;
  | A a -> print_a a

let print title t (ctxt_in, ctxt_out) =
  print_newline ();
  print_endline ("  " ^ title);
  print_string "    exp: "; print_t t;
  print_string "    in : "; print_e (plug_in_handle hole ctxt_in);
  print_string "    out: "; print_e (plug_all hole (CId, ctxt_out))
    
let rec eval (exp : e) ((ctxt_in, ctxt_out) : ctxt) : v =
  print "eval" (E exp) (ctxt_in, ctxt_out);
  match exp with
  | Val (v) -> v
  | App (e1, e2) ->
    begin
      let v2 = eval e2 (CApp2 (e1, ctxt_in), ctxt_out) in
      print "apply_in" (V v2) (CApp2 (e1, ctxt_in), ctxt_out);
      let v1 = eval e1 (CApp1 (v2, ctxt_in), ctxt_out) in
      print "apply_in" (V v1) (CApp1 (v2, ctxt_in), ctxt_out);
      match v1 with
      | Fun (x, e) ->
        let reduct = subst e [(x, v2)] in
        eval reduct (ctxt_in, ctxt_out)
      | Cont (x, ctxt_in') ->
        let reduct = plug_in_handle (Val v2) (ctxt_in' CId) in
        eval reduct (ctxt_in, ctxt_out)
      | _ -> failwith "type error"
    end
  | Op (name, e) ->
    let v = eval e (COp (name, ctxt_in), ctxt_out) in
    print "apply_in" (V v) (COp (name, ctxt_in), ctxt_out);
    raise (Call (name, v, ctxt_in))
  | With (e1, e2) ->
    let v1 = eval e1 (CWith (e2, ctxt_in), ctxt_out) in
    let h = match v1 with
      | Handler (h) -> h
      | _ -> failwith "type error" in
    let a2 =
      try Return (eval e2 (CId, DHandle (ctxt_in, h, ctxt_out)))
      with Call (name, v, ctxt_in') -> OpCall (name, v, ctxt_in') in
    print "apply_out" (A a2) (CId, DHandle (ctxt_in, h, ctxt_out));
    apply_handler (ctxt_in, ctxt_out) h a2

and apply_handler ((ctxt_in, ctxt_out) : ctxt) (h : h) (a : a) : v =
  print_newline ();
  print_endline "  apply_out";
  print_string "    exp: "; print_a a;
  print_string "    in : "; print_e (plug_in_handle hole ctxt_in);
  print_string "    out: "; print_e (plug_all hole (CId, ctxt_out));
  match a with
  | Return v ->
    begin match h.return with
      | None -> v
      | Some (x, e) ->
        let reduct = subst e [(x, v)] in
        eval reduct (ctxt_in, ctxt_out) end
  | OpCall (name, v, ctxt_in') ->
    begin match search_op name h with
      | None ->
        raise (Call (name, v, CCall (ctxt_in, h, ctxt_in')))
      | Some (x, k, e) ->
        let new_var = gen_var_name () in
        let cont_value =
          Cont (new_var,
                fun ctxt_outer -> CCall (ctxt_in, h, ctxt_in')) in
        let reduct = subst e [(x, v); (k, cont_value)] in
        eval reduct (CId, ctxt_out)
    end

let stepper (e : e) : a =
  let result = 
  try Return (eval e (CId, DId))
  with Call (name, v, ctxt_in) -> OpCall (name, v, ctxt_in)
  in
  print "apply_out" (A result) (CId, DId);
  result
