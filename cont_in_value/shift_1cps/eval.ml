open Syntax
open Value

let id : v -> v = fun v -> v

let rec f (t : t) (env : Env.t) (c : c) : v = match t with
  | Var (x) -> c (Env.get env x)
  | Fun (x, t) ->
    c (VFun (fun v c' -> f t (Env.add env x v) c'))
  | App (t1, t2) ->
    f t2 env (fun v2 ->
        f t1 env (fun v1 ->
            match v1 with
            | VFun (f) -> f v2 c
            | VCont (c') -> c (c' v2)
          )
      )
  | Shift (x, t) ->
    f t (Env.add env x (VCont (c))) id
  | Reset (t) ->
    c (f t env id)

let eval (t : t) = f t Env.empty id
    

