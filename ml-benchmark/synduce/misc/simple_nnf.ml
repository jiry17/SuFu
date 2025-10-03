type formula =
  | FLit of bool
  | FAnd of formula * formula
  | FOr of formula * formula
  | FNot of formula

let rec run = function
  | FLit b -> b
  | FAnd (x, y) -> run x && run y
  | FOr (x, y) -> run x || run y
  | FNot x -> not (run x)

type nnf_formula =
  | NFNegLit of bool
  | NFLit of bool
  | NFAnd of nnf_formula * nnf_formula
  | NFOr of nnf_formula * nnf_formula

val repr: nnf_formula -> formula compress 
let rec repr = function
  | NFLit b -> FLit b
  | NFNegLit b -> FNot (FLit b)
  | NFAnd (x, y) -> FAnd (repr x, repr y)
  | NFOr (x, y) -> FOr (repr x, repr y)

let prog e = run (repr e)