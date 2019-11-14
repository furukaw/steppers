open Syntax

type v = VFun of (v -> c -> v)
       | VCont of c

and c = v -> v

let v_to_string : v -> string = function
  | VFun (_) -> "<fun>"
  | VCont (_) -> "<cont>"

let print_v (v : v) : unit =
  print_endline (v_to_string v)

