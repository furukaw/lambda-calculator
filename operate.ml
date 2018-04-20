open Syntax
open State
open Set
open Env

(* 括弧をつけまくる *)
let rec par_max (term : Syntax.lam) : Syntax.lam = match term with
  | Var _ -> term
  | Abs (x, xs, t, _) ->
    begin match xs with
        [] -> Abs (x, [], par_max t, true)
      | first :: rest ->
        Abs (x, [], par_max (Abs (first, rest, t, true)), true)
    end
  | App (t1, t2, _) -> App (par_max t1, par_max t2, true)
                         
(* 括弧を省略する *)
let par_min (term : Syntax.lam) : Syntax.lam = 
  let rec par_min (term : Syntax.lam) : Syntax.lam = match term with
    | Var _ -> term
    | App (App (t1, t2, _), t3, p) ->
      App (par_min (App (t1, t2, false)), par_min t3, p)
    | Abs (x, xs, App (t1, t2, _), p) ->
      Abs (x, xs, par_min (App (t1, t2, false)), p)
    | Abs (x, xs, Abs (x', xs', t', _), p) ->
      par_min (Abs (x, xs @ (x' :: xs'), t', p))
    | App (t1, t2, p) -> App (par_min t1, par_min t2, p)
    | Abs (x, xs, t, p) -> Abs (x, xs, par_min t, p)
  in
  let result = par_min term in
  match result with
  | Abs (x, xs, t, _) -> Abs (x, xs, t, false)
  | App (t1, t2, _) -> App (t1, t2, false)
  | _ -> result

(* 構造的に等しいかどうか *)
let rec equal (term1 : Syntax.lam) (term2 : Syntax.lam) : bool =
  match (term1, term2) with
  | (Var x1, Var x2) -> x1 = x2
  | (Abs (x1, xs1, t1, _), Abs (x2, xs2, t2, _)) ->
    x1 = x2 && xs1 = xs2 && equal t1 t2
  | (App (t11, t12, _), App (t21, t22, _)) -> equal t11 t21 && equal t12 t22
  | _ -> false

(* 自由変数の集合を得る *)
let rec fv (term : Syntax.lam) : string set = match term with
  | Var x -> [x]
  | Abs (x, xs, t, _) -> List.fold_left minus (fv t) (x :: xs)
  | App (t1, t2, _) -> u (fv t1) (fv t2)

(* 閉じた項かどうかを返す *)
let is_closed (term : Syntax.lam) : bool = match fv term with
  | [] -> true
  | _ -> false

(* 名前変更する *)
let rec rename (term : Syntax.lam) (before : string) (after: string) : Syntax.lam =
  match term with
  | Var x -> if x = before then Var after else term
  | Abs (x, xs, t, p) ->
    if List.mem before (x :: xs) then term
    else Abs (x, xs, rename t before after, p)
  | App (t1, t2, p) -> App (rename t1 before after, rename t2 before after, p)

type trans_state = Syntax.lam * int * (string, string) Env.t
                    
(* 変数名を全部自然数に直す *)
let normalize (term : Syntax.lam) : Syntax.lam =
  let rec normalize ((term, num, env) : trans_state) : trans_state = match term with
    | Var x -> ((try Var (Env.getr x env) with Not_found -> term), num, env)
    | Abs (x, xs, t, p) ->
      let (new_env, new_num) = Env.extendInc env (x :: xs) num in
      let (t1, num1, env1) = normalize (t, new_num, new_env) in
      (Abs (x, xs, t1, p), num1, env1)
    | App (t1, t2, p) ->
      let (t11, num1, env1) = normalize (t1, num, env) in
      let (t22, num2, env2) = normalize (t2, num1, env1) in
      (App (t11, t22, p), num2, env2)
  in
  match normalize (term, 0, []) with (term, _, _) -> term

(* α同値かどうか *)
let alpha_eq (term1 : Syntax.lam) (term2 : Syntax.lam) : bool =
  let nl1 = par_max (normalize term1) in
  let nl2 = par_max (normalize term2) in
  equal nl1 nl2
    
(* 命令を実行する *)
let eval (input : Syntax.t) (state : State.t) : State.t =
  match (input, state) with
  | (Lam lam, Nothing) -> Term lam
  | (Max, Term term) -> Term (par_max term)
  | (Min, Term term) -> Term (par_min term)
  | (FV, Term term) -> VarSet (fv term)
  | (IsClosed, Term term) -> Bool (is_closed term)
  | (Rename (before, after), Term term) -> Term (rename term before after)
  | (And newterm, Term term) -> TermSet (term :: newterm :: [])
  | (And newterm, TermSet list) -> TermSet (list @ [newterm])
  | (AlphaEq, TermSet (first :: second :: rest)) ->
    Bool (List.for_all (alpha_eq first) (second :: rest))
  | (Reset, _) -> Nothing
  | _ -> print_endline "### Error ###"; state
