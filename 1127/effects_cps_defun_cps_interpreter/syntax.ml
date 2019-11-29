(* 値の型 *)
type v = Var of string       (* x *)
       | Fun of string * e  (* fun x -> e *)
       | Cont of cont_in * (string * e)
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

and a = Return of v
      | OpCall of string * v * ctxt * cont_in

and cont_in = FId
         | FApp2 of e * ctxt * cont_in        (* e1, ctxt_in, ctxt_out, cont_in *)
         | FApp1 of v * ctxt * cont_in        (* v2, ctxt_in, ctxt_out, cont_in *)
         | FOp of string * ctxt_in * cont_in  (* op, ctxt_in, cont_in *)
         | FWith of e * ctxt * cont_in        (* e2, ctxt_in, ctxt_out, cont_in *)
         | FCont of cont_in * cont_in

and cont_out = a -> a

(* コンテキストフレームの型 *)
and frame = CApp2 of e     (* F[e [.]] *)
          | CApp1 of v     (* F[[.] v] *)
          | COp of string  (* F[op [.]] *)
          | CWith of e     (* F[with [.] handle e] *)

and handle_frame = CHandle of h * frame list

and ctxt_in = frame list
and ctxt_out = handle_frame list
and ctxt = ctxt_in * ctxt_out

(* コンテキストを１層深くする関数 *)
let add_frame (frame : frame) (frames : ctxt_in) : ctxt_in =
  frame :: frames

let add_handle (h : h) (frames : ctxt_in) (handles : ctxt_out) : ctxt_out =
  CHandle (h, frames) :: handles

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug_in_handle (e : e) (frames : frame list) : e = match frames with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CApp2 (e1) -> App (e1, e)
      | CApp1 (v2) -> App (e, Val v2)
      | COp (op) -> Op (op, e)
      | CWith (e2) -> With (e, e2)
    in
    plug_in_handle plugged rest

let rec plug_all (e : e) ((frames, handles) : ctxt) : e =
  let e_in_handle = plug_in_handle e frames in
  match handles with
  | [] -> e_in_handle
  | (CHandle (h, next_frames)) :: outer_handles ->
    let e_handle = With (Val (Handler h), e_in_handle) in
    plug_all e_handle (next_frames, outer_handles)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Cont (_, (x, e)) -> "(fun " ^ x ^ " => " ^ e_to_string e ^ ")"
  | Handler (h) -> "(handler {" ^ h_to_string h ^ "})"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Op (op, e1) -> "(" ^ op ^ " " ^ e_to_string e1 ^ ")"
  | With (e1, e2) ->
    "(with " ^ e_to_string e1 ^ " handle " ^ e_to_string e2 ^ ")"

and h_to_string : h -> string = function
  | {return; ops} ->
    let op_strs =
      List.map
        (fun (op, x, k, e) -> op ^ "(" ^ x ^ "; " ^ k ^ ") -> " ^ e_to_string e)
        ops in
    let strs = match return with
      | None -> op_strs
      | Some (x, e) -> ("return " ^ x ^ " -> " ^ e_to_string e) :: op_strs in
    match strs with
    | [] -> ""
    | first :: rest ->
    List.fold_left
      (fun x y -> x ^ ", " ^ y)
      first
      rest

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)
