open Syntax

let rec subst_v (v : v) (pairs : (string * v) list) : v = match v with
  | Var (x) -> (try List.assoc x pairs with Not_found -> v)
  | VFun (f, (x, e)) -> if List.mem_assoc x pairs then v else VFun (f, (x, subst e pairs))

and subst (e : e) (pairs : (string * v) list) : e = match e with
  | Val (v) -> Val (subst_v v pairs)
  | Fun (x, e) -> if List.mem_assoc x pairs then e else Fun (x, subst e pairs)
  | App (e1, e2) -> App (subst e1 pairs, subst e2 pairs)
  | Raise (e1) -> Raise (subst e1 pairs)
  | Try (e1, x, e2) ->
    Try (subst e1 pairs, x,
         if List.mem_assoc x pairs then e2 else subst e2 pairs)
