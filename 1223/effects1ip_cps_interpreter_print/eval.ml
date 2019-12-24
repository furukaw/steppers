open Syntax
open Util
open Memo

let id_in : cont_in = fun v ->
  print_newline ();
  print_endline "  apply_in id";
  print_string "    exp: "; print_v v;
  Return v

(* op とハンドラを受け取って、ハンドラで op が定義されていればその情報を返す *)
let search_op (op : string) ({ops} : h) : (string * string * e) option =
  try
    let (_, x, k, e) = List.find (fun (name, x, k, e) -> name = op) ops in
    Some (x, k, e)
  with Not_found -> None

let rec eval (exp : e) (cont_in : cont_in) : a =
  print_newline ();
  print_endline "  eval";
  print_string "    exp: "; print_e exp;
  match exp with
  | Val (v) -> cont_in v
  | App (e1, e2) ->
    eval e2 (fun v2 ->
        print_newline ();
        print_endline "  apply_in app2";
        print_string "    exp: "; print_v v2;
        eval e1 (fun v1 ->
            print_newline ();
            print_endline "  apply_in app1";
            print_string "    exp: "; print_v v1;
            match v1 with
            | Fun (x, e) ->
              let reduct = subst e [(x, v2)] in
              eval reduct cont_in
            | Cont (cont_in') ->
              cont_in' cont_in v2
            | _ -> failwith "type error"
          )
      )
  | Op (name, e) ->
    eval e (fun v ->
        print_newline ();
        print_endline "  apply_in op";
        print_string "    exp: "; print_v v;
        OpCall (name, v, cont_in))
  | With (e1, e2) ->
    eval e1 (fun v1 ->
        print_newline ();
        print_endline "  apply_in with";
        print_string "    exp: "; print_v v1;
        let h = match v1 with
          | Handler (h) -> h
          | _ -> failwith "type error" in
        let a = eval e2 id_in in
        g cont_in h a
      )

and g (cont_last : cont_in) (h : h) (a : a) : a =
  print_newline ();
  print_endline "  apply_out handle";
  print_string "    exp: "; print_a a;
  match a with
  | Return v ->
    begin match h.return with
      | None -> cont_last v
      | Some (x, e) -> eval (subst e [(x, v)]) cont_last end
  | OpCall (name, v, cont_in') ->
    begin match search_op name h with
      | None ->
        OpCall (name, v, (fun v -> g cont_last h (cont_in' v)))
      | Some (x, k, e) ->
        let reduct =
          subst e [(x, v);
                   (k, Cont (fun id_in -> fun v -> g id_in h (cont_in' v)))] in
        eval reduct cont_last
    end

hlet stepper (e : e) : v =
  let a = eval e id_in in
  print_newline ();
  print_endline "  apply_out id";
  print_string "    exp: "; print_a a;
  match a with
  | Return v -> v
  | OpCall (name, _, _) -> failwith ("no handlers for " ^ name)
