(* 値の型 *)
type v = Var of string       (* x *)
       | Fun of string * e  (* fun x -> e *)
       | Cont of cont * (string * e)
       | Handler of h

(* 式の型 *)
and e = Val of v           (* v *)
      | App of e * e       (* e e *)
      | Op of string * e   (* op e *)
      | With of e * e      (* with e handle e *)

and h = {
  return : (string * e) option;
  ops : (string * string * string * e) list;
}

and cont = v -> v

(* コンテキストフレームの型 *)
and frame = CApp2 of e     (* F[e [.]] *)
          | CApp1 of v     (* F[[.] v] *)
          | COp of string  (* F[op [.]] *)

(* コンテキストの型 *)
and ctxt = frame list

(* コンテキストを１層深くする関数 *)
let add_frame (frame : frame) (ctxt : ctxt) : ctxt = frame :: ctxt

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug_all (e : e) (ctxt : ctxt) = match ctxt with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CApp2 (e1) -> App (e1, e)
      | CApp1 (v2) -> App (e, Val v2)
      | COp (op) -> Op (op, e) in
    plug_all plugged rest

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Cont (_, (x, e)) -> "(fun " ^ x ^ " => " ^ e_to_string e ^ ")"
  | Handler (h) -> h_to_string h

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Op (op, e1) -> "(" ^ op ^ " " ^ e_to_string e1 ^ ")"
  | With (e1, e2) ->
    "(with " ^ e_to_string e1 ^ " handle " ^ e_to_string e2 ^ ")"

and h_to_string : h -> string = function
  | {return; ops} ->
    let return_str = match return with
      | None -> ""
      | Some (x, e) -> "return " ^ x ^ " -> " ^ e_to_string e in
    let op_strs =
      List.map
        (fun (op, x, k, e) -> op ^ "(" ^ x ^ "; " ^ k ^ ") -> " ^ e_to_string e)
        ops in
    List.fold_left
      (fun x y -> x ^ ", " ^ y)
      return_str
      op_strs

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)
