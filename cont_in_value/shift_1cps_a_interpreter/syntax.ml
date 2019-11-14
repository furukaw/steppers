
type t = Val of v
       | App of t * t
       | Shift of string * t
       | Reset of t

and v = Var of string
      | Fun of string * t
      | Cont of string * t

let rec t_to_string : t -> string = function
  | Val (v) -> v_to_string v
  | App (t1, t2) -> "(" ^ t_to_string t1 ^ " " ^ t_to_string t2 ^ ")"
  | Shift (x, t) -> "(shift (fun " ^ x ^ " -> " ^ t_to_string t ^ "))"
  | Reset (t) -> "(reset (fun () -> " ^ t_to_string t ^ "))"

and v_to_string : v -> string = function
  | Var (x) -> x
  | Fun (x, t) -> "(fun " ^ x ^ " -> " ^ t_to_string t ^ ")"
  | Cont (x, t) -> "(fun " ^ x ^ " => " ^ t_to_string t ^ ")"

let print_t (t : t) : unit =
  print_endline (t_to_string t)

let print_v (v : v) : unit =
  print_endline (v_to_string v)
