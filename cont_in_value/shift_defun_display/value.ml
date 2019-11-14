open Syntax

type v = VFun of ((v -> c -> v) * (string * t))
       | VCont of (c * (string * t))

and c = v -> v

let v_to_t : v -> t = function
  | VFun (_, (x, t)) -> Fun (x, t)
  | VCont (_, (x, t)) -> Fun (x, t)

let v_to_string : v -> string = function
  | VFun (_, (x, t)) -> t_to_string (Fun (x, t))
  | VCont (_, (x, t)) -> "(fun " ^ x ^ " => " ^ t_to_string t ^ ")"

let print_v (v : v) : unit =
  print_endline (v_to_string v)

