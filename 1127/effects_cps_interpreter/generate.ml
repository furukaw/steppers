#use "syntax.ml";;

Random.init 0;;

let names =
  let rec a_to_w i =
    if i > 119 then [] else Char.escaped (Char.chr i) :: a_to_w (i + 1) in
  let rec x_to_z i =
    if i > 122 then a_to_w 97
    else Char.escaped (Char.chr i) :: x_to_z (i + 1) in
  x_to_z 120

let rec f (vars : string list) (n : int) : e list =
  if n <= 1
  then
    let e1 = Val (Var (List.nth vars (Random.int (List.length vars)))) in
    let e2 = Val (Handler {return = None; ops = []}) in
  else
    let new_var1 = List.nth names (List.length vars) in
    let new_vars1 = new_var1 :: vars in
    let child1 = f new_vars1 (n - 1) in
    let new_var2 = List.nth names (List.length new_vars1) in
    let new_vars2 = new_var2 :: new_vars1 in
    let child2 = f new_vars2 (n - 1) in
    let es1 = List.map (fun child -> Val (Fun (new_var, child))) in
    let es2 = Val (Handler {return = None; ops = 
