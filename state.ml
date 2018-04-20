 open Syntax

(* State.t : 状態を表す型 *)
type t = Term of Syntax.lam
       | TermSet of Syntax.lam list
       | VarSet of string list
       | Bool of bool
       | Num of int
       | Nothing

(* State.list_to_string : リストを文字列にする型 *)
let list_to_string list to_string =
  let f one rest = to_string one ^ "\n  " ^ rest in
  List.fold_right f list " }"

(* State.to_string : 状態を文字列にする関数 *)
let rec to_string (state : t) : string = match state with
  | Term t -> Syntax.to_string t
  | TermSet [] -> "{} (no terms)"
  | TermSet list -> "{ " ^ list_to_string list Syntax.to_string
  | VarSet [] -> "{} (no variables)"
  | VarSet (first :: rest) ->
    "{" ^ List.fold_left (fun x y -> x ^ ", " ^ y) first rest ^ "}"
  | Bool b -> string_of_bool b
  | Num n -> string_of_int n
  | Nothing -> "empty"

(* State.abss : λ項の「λ」を数える関数 *)
let rec abss (term : Syntax.lam) : int = match term with
  | Var _ -> 0
  | Abs (_, _, t, _) -> 1 + abss t
  | App (t1, t2, _) -> abss t1 + abss t2

(* State.max_length : リスト内のλ項の最大文字数を返す関数 *)
let rec max_length list num = match list with
    [] -> num
  | first :: rest ->
    max num (String.length (Syntax.to_string first) - abss first)

(* State.space : スペースを並べる関数 *)
let rec space (num : int) : string =
  if num < 1 then "" else " " ^ space (num - 1)

(* State.terms_to_string : λ項の集合を文字列にする関数 *)
let rec terms_to_string (list : Syntax.lam list) (num : int) : string =
  match list with
    [] -> ""
  | first :: rest ->
    let first_string = Syntax.to_string first in
    let first_length = String.length first_string in
    let spaces = space (num - first_length) in
    "|  " ^ first_string ^ spaces ^ "  |\n" ^ terms_to_string rest num
  
(* State.horizon : ハイフンを並べる関数 *)
let rec horizon (n : int) : string =
  if n < 1 then "" else "-" ^ horizon (n - 1)

(* State.print : 状態を出力する関数 *)
let print (state : t) : unit =
  match state with
  | TermSet list ->
    let rows = max_length list 0 in
    let line = "+" ^ horizon (rows + 4) ^ "+" in
    print_endline line;
    print_string (terms_to_string list rows);
    print_endline line
  | _ ->
    let result = to_string state in
    let lambdas = match state with
      | Term term -> abss term
      | _ -> 0 in
    let l = String.length result - lambdas in
    let line = "+" ^ horizon (l + 4) ^ "+" in
    print_endline line;
    print_string "|  ";
    print_string result;
    print_string "  |\n";
    print_endline line
