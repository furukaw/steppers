type t = Var of string
       | Fun of string * t
       | App of t * t
       | Shift of string * t
       | Reset of t

let rec t_to_string : t -> string = function
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ t_to_string e ^ ")"
  | App (e1, e2) -> "(" ^ t_to_string e1 ^ " " ^ t_to_string e2 ^ ")"
  | Shift (k, e) -> "(shift (fun " ^ k ^ " -> " ^ t_to_string e ^ "))"
  | Reset (e) -> "(reset (fun () -> " ^ t_to_string e ^ "))"

let print_e (e : t) : unit =
  print_endline (t_to_string e)
