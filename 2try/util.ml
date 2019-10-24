open Syntax

(* v の中の自由変数 var を value に置換する *)
let rec subst_value (v : v_t) ((var, value) as pair) : v_t = match v with
  | Var (x) -> if x = var then value else v
  | Fun (x, e) -> if x = var then v else Fun (x, subst e pair)

(* e の中の自由変数を値に置換する *)
and subst (e : e_t) ((var, value) as pair : string * v_t) : e_t =
  match e with
  | Val (v) -> Val (subst_value v pair)
  | App (e1, e2) -> App (subst e1 pair, subst e2 pair)
  | Raise (e0) -> Raise (subst e0 pair)
  | Try (e0, x, e1) ->
    Try (subst e0 pair, x, if x = var then e1 else subst e1 pair)

