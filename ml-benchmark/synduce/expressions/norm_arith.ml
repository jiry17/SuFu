type arith_expr =
  | Int of int
  | Plus of arith_expr * arith_expr
  | Minus of arith_expr * arith_expr

type norm_expr =
  | NInt of int
  | NPlus of norm_expr * norm_expr

val repr: norm_expr -> arith_expr compress
let rec repr = function
  | NInt i -> Int i
  | NPlus (a, b) -> Plus (repr a, repr b)

let rec spec = function
  | Int i -> i
  | Plus (a, b) -> spec a + spec b
  | Minus (a, b) -> spec a - spec b

let program e = spec (repr e)