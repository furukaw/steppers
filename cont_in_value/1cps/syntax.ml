(* 値の型 *)
type v_t = Var of string        (* x *)
         | Fun of string * e_t  (* fun x -> e *)
         | Handler of h_t
         | Cont of (string * cont_t * ctx_t)

and h_t = {
  return : (string * e_t) option;  (* handler {return x -> er, *)
  ops : (string * string * string * e_t) list  (* op1(x; k) -> c1, ...} *)
}

(* 式の型 *)
and e_t = Val of v_t                 (* v *)
        | App of e_t * e_t           (* e e *)
        | Op of string * e_t         (* op e *)
        | With of e_t * e_t          (* with e handle e *)

and a_t = Value of v_t
        | OpCall of string * v_t * ctx_t * cont_t

and cont_t = v_t -> a_t

and cf_t = CAppR of e_t   (* F[e [.]] *)
         | CAppL of v_t   (* F[[.] v] *)
         | COp of string  (* F[op [.]] *)
         | CWith of e_t   (* F[with [.] handle e] *)

and ce_t = CHandle of cf_t list * h_t  (* E[F[with v handle [.]]] *)

(* コンテキストの型 *)
and ctx_t = cf_t list * ce_t list  (* E[F[]] *)

let empty_context = ([], [])

(* コンテキストを１層深くする関数 *)
let add_frame (frame : cf_t) ((frames, handles) : ctx_t) : ctx_t =
  (frame :: frames, handles)

(* handle フレームを追加したコンテキストを返す *)
let add_handle (h : h_t) ((frames, handles) : ctx_t) =
  ([], (CHandle (frames, h)) :: handles)

(* 式とコンテキストフレームのリストを受け取って再構成した式を返す *)
let rec plug_frames (e : e_t) (frames : cf_t list) : e_t = match frames with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CAppR (e1) -> App (e1, e)
      | CAppL (v2) -> App (e, Val v2)
      | COp (name) -> Op (name, e)
      | CWith (e1) -> With (e1, e) in
    plug_frames plugged rest

(* op名と式とコンテキストを受け取ってopが捕捉されるhandleの内部の式を返す *)
let rec plug_in_handles name (e : e_t) ((frames, handles) : ctx_t) : e_t =
  let e_in_handle = plug_frames e frames in
  match handles with
  | [] -> e_in_handle
  | CHandle (outer_frames, ({return; ops} as h)) :: outer_handles ->
    if List.exists (fun (op, x, k, e) -> op = name) ops
    then e_in_handle
    else plug_in_handles
        name (With (Val (Handler h), e_in_handle)) (outer_frames, outer_handles)

(* 式とコンテキストを受け取って再構成した式を返す *)
let rec plug (e : e_t) ((frames, handles) : ctx_t) : e_t =
  let e_in_handle = plug_frames e frames in
  match handles with
  | [] -> e_in_handle
  | CHandle (outer_frames, h) :: outer_handles ->
    plug (With (Val (Handler h), e_in_handle)) (outer_frames, outer_handles)

(* 値を文字列にする関数 *)
let rec v_to_string : v_t -> string = function
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Handler (h) -> "(handler {" ^ h_to_string h ^ "})"
  | Cont (var, cont, ctxt) -> "(fun " ^ var ^ " => " ^
                              e_to_string (plug (Val (Var var)) ctxt) ^ ")"

and h_to_string : h_t -> string = fun {return; ops} ->
  let return_str : string list = match return with
    | None -> []
    | Some (x, er) -> ["return " ^ x ^ " -> " ^ e_to_string er] in
  let op_to_string (name, x, k, e) =
    name ^ "(" ^ x ^ "; " ^ k ^ ") -> " ^ e_to_string e in
  let ops_str = List.map op_to_string ops in
  match (return_str @ ops_str) with
  | [] -> ""
  | first :: rest ->
    List.fold_left (fun x y -> x ^ ", " ^ y) first rest

(* 式を文字列にする関数 *)
and e_to_string : e_t -> string = function
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Op (name, e) -> "(" ^ name ^ " " ^ e_to_string e ^ ")"
  | With (e1, e2) ->
    "(with " ^ e_to_string e1 ^ " handle " ^ e_to_string e2 ^ ")"

let a_to_string : a_t -> string = function
  | Value v -> v_to_string v
  | OpCall (name, _, _, _) -> "Error (no handler for " ^ name ^ ")"

(* 式を標準出力する *)
let print_e (e : e_t) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v_t) : unit =
  print_endline (v_to_string v)

let print_a (a : a_t) : unit =
  print_endline (a_to_string a)
