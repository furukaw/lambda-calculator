(* Syntax.t : λ項を表す型 *)
type lam = Var of string
         | App of lam * lam * bool
         | Abs of string * string list * lam * bool
                  
(* Syntax.to_string : λ項を文字列に変換する *)
let rec to_string (term : lam) : string =
  let app_to_string t1 t2 = to_string t1 ^ " " ^ to_string t2 in
  let abs_to_string x xs t = "λ" ^ x ^ List.fold_right (^) xs ""
                             ^ ". " ^ to_string t in
  let add_par s = "(" ^ s ^ ")" in
  match term with
  | Var x -> x
  | App (t1, t2, true) -> add_par (app_to_string t1 t2)
  | App (t1, t2, false) -> app_to_string t1 t2
  | Abs (x, xs, t, true) -> add_par (abs_to_string x xs t)
  | Abs (x, xs, t, false) -> abs_to_string x xs t
                               
(* Syntax.print : λ項を出力する *)
let print_lam (term : lam) : unit = print_endline (to_string term)
    
(* 入力内容を表す型 *)
type t = Lam of lam
       | Max
       | Min
       | FV
       | IsClosed
       | Rename of string * string
       | And of lam
       | AlphaEq
       | Reset
