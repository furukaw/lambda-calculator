(* 環境の型 *)
type ('a, 'b) t = ('a * 'b) list

(* 環境に一組追加する *)
let rec extend (env : ('a, 'b) t) (left : 'a) (right : 'b) : ('a, 'b) t =
  (left, right) :: env

let rec extendInc (env : (string, 'b) t) (rights : 'a list) (num : int)
  : (string, 'b) t * int =
  match rights with
    [] -> (env, num)
  | first :: rest -> extendInc ((string_of_int num, first) :: env) rest (num + 1)

(* 左の要素と環境から右の要素を検索する *)
let rec getr (left : 'a) (env : ('a, 'b) t) : 'b = match env with
    [] -> raise Not_found
  | (first, right) :: rest -> if first = left then right else getr left rest
