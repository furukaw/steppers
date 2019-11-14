
type t = Val of v
       | Fun of string * t
       | App of t * t
       | Shift of string * t
       | Reset of t

and v = Var of string
      | VFun of ((v -> cont -> v) * (string * t))
      | VCont of cont * (string * t)

and cont = v -> v

let rec t_to_string : t -> string = function
  | Val (v) -> v_to_string v
  | Fun (x, t) -> "(fun " ^ x ^ " -> " ^ t_to_string t ^ ")"
  | App (t1, t2) -> "(" ^ t_to_string t1 ^ " " ^ t_to_string t2 ^ ")"
  | Shift (x, t) -> "(shift (fun " ^ x ^ " -> " ^ t_to_string t ^ "))"
  | Reset (t) -> "(reset (fun () -> " ^ t_to_string t ^ "))"

and v_to_string : v -> string = function
  | Var (x) -> x
  | VFun (_, (x, t)) -> "(fun " ^ x ^ " -> " ^ t_to_string t ^ ")"
  | VCont (_, (x, t)) -> "(fun " ^ x ^ " => " ^ t_to_string t ^ ")"

let print_t (t : t) : unit =
  print_endline (t_to_string t)

let print_v (v : v) : unit =
  print_endline (v_to_string v)
