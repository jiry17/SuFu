type arith_expr =
  | Int of int
  | Plus of arith_expr * arith_expr
  | Minus of arith_expr * arith_expr

type psum_expr =
  | NInt of int
  | NPlus of int * psum_expr * psum_expr
  | NMinus of int * psum_expr * psum_expr

let max a b = if a > b then a else b

val repr: psum_expr -> arith_expr compress
let rec repr = function
  | NInt i -> Int i
  | NPlus (s, a, b) -> Plus (repr a, repr b)
  | NMinus (s, a, b) -> Minus (repr a, repr b)

let rec sum = function
  | NInt i -> i
  | NPlus (s, a, b) -> sum a + sum b
  | NMinus (s, a, b) -> sum a - sum b

let rec is_memo_psum = function
  | NInt i -> true
  | NPlus (s, a, b) -> s >= sum a + sum b && is_memo_psum a && is_memo_psum b
  | NMinus (s, a, b) -> s >= sum a - sum b && is_memo_psum a && is_memo_psum b

let rec spec = function
  | Int i -> (i, i)
  | Plus (a, b) ->
    (match spec a with (asum, am) -> 
      match spec b with (bsum, bm) -> 
        (asum + bsum, max (asum + bsum) (max am bm)))
  | Minus (a, b) ->
    (match spec a with (asum, am) -> 
      match spec b with (bsum, bm) -> 
        (asum - bsum, max (asum - bsum) (max am bm)))

let prog e = spec (repr e)