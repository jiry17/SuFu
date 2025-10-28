type aexpr =
  | Int of int
  | Plus of aexpr * aexpr
  | Minus of aexpr * aexpr

type nexpr =
  | Nint of int
  | Nplus of int * nexpr * nexpr
  | Nminus of int * nexpr * nexpr

let rec repr e =
  match e with
  | Nint a -> Int a
  | Nplus (_, a, b) -> Plus (repr a, repr b)
  | Nminus (_, a, b) -> Minus (repr a, repr b)

let memo e =
  match e with
  | Nint a -> a
  | Nplus (a, _, _) -> a
  | Nminus (a, _, _) -> a

let rec is_memo e =
  match e with
  | Nint _ -> true
  | Nplus (n, e1, e2) -> (n == memo e1 + memo e2) && (is_memo e1 && is_memo e2)
  | Nminus (n, e1, e2) -> (n == memo e1 - memo e2) && (is_memo e1 && is_memo e2)

let rec spec e =
  match e with
  | Int a -> a
  | Plus (e1, e2) -> spec e1 + spec e2
  | Minus (e1, e2) -> spec e1 - spec e2

val target: nexpr -> nexpr compress
let rec target e =
  match e with
  | Nint a -> Nint a
  | Nplus (_, _, _) -> e
  | Nminus (_, _, _) -> e

let rec gen e =
  match e with
  | Int a -> Nint a
  | Plus (a, b) ->
    let res = (gen a, gen b) in (
      match res with
      | (r1, r2) -> Nplus (memo r1 + memo r2, r1, r2)
    )
  | Minus (a, b) ->
      let res = (gen a, gen b) in
      match res with
      | (r1, r2) -> Nminus (memo r1 - memo r2, r1, r2)

let program e =
  let inp = gen e in
  if is_memo inp then spec (repr (target inp)) else 0