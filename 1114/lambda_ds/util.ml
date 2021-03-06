open Syntax

let rec subst_v (v : v) (pairs : (string * v) list) : v = match v with
  | Var (x) -> (try List.assoc x pairs with Not_found -> v)
  | VFun (x, e) -> if List.mem_assoc x pairs then v else VFun (x, subst e pairs)

and subst (e : e) (pairs : (string * v) list) : e = match e with
  | Val (v) -> Val (subst_v v pairs)
  | Fun (x, e) -> if List.mem_assoc x pairs then e else Fun (x, subst e pairs)
  | App (e1, e2) -> App (subst e1 pairs, subst e2 pairs)
                      
