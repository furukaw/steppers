open Syntax
open Util
open Context

exception Op of (string * v_t * ctx_t)
exception TypeError of (c_t * string)

let rec f (com : c_t) (ctxt : ctx_t) : v_t = match com with
  | Val (v) -> v
  | App (c1, c2) ->
    let v1 = f c1 (add_app1 c2 ctxt) in
    let v2 = f c2 (add_app2 v1 ctxt) in
    let redex = App (Val v1, Val v2) in
    let reduct = match v1 with
      | Fun (x, c_fun) -> subst c_fun (x, v2)
      | _ -> raise (TypeError (redex, "app")) in
    memo redex reduct ctxt;
    let result = f reduct ctxt in
    result
  | If (c0, c1, c2) ->
    let v0 = f c0 (add_if c1 c2 ctxt) in
    let redex = If (Val v0, c1, c2) in
    let reduct = match v0 with
      | True -> c1
      | False -> c2
      | _ -> raise (TypeError (redex, "if")) in
    memo redex reduct ctxt;
    let result = f reduct ctxt in
    result
  | Op (op, c) ->
    let v = f c (add_op op ctxt) in
    
    raise (Op (op, v, ctxt))
      
