type list_ = Elt of int | Cons of int * list_
type pos = One of unit | S of pos
type map = Value of int * pos | Node of int * map * map

let min a b = if a < b then a else b
let max a b = if a > b then a else b

let rec min_key m =
  match m with
  | Value (k, v) -> k
  | Node (a, l, r) -> min (min_key l) (min_key r)

let rec max_key m =
  match m with
  | Value (k, v) -> k
  | Node (a, l, r) -> max (max_key l) (max_key r)

let rec is_map m =
  match m with
  | Value (k, v) -> true
  | Node (a, l, r) -> (max_key l) < a && a <= (min_key r) && is_map l && is_map r

let rec cat x y =
  match x with
  | Cons (h, t) -> Cons (h, cat t y)
  | Elt w -> Cons (w, y)

let repeat w =
  let rec f n =
    match n with
    | One _ -> Elt w
    | S n' -> Cons (w, f n')
  in
  f

let rec repr m =
  match m with
  | Value (k, v) -> (repeat k) v
  | Node (a, l, r) -> cat (repr l) (repr r)

let count w =
  let rec f xs =
    match xs with
    | Elt h -> if h = w then 1 else 0
    | Cons (h, t) -> (if h = w then 1 else 0) + f t
  in
  f

let spec xs =
  let rec f l =
    match l with
    | Elt v -> (1, v)
    | Cons (h, t) ->
        let cnt = count h l in
        let res = f t in
        match res with
        | a, b ->
            if cnt > a then (cnt, h) else res
  in
  match f xs with
  | a, b -> b

let rec p2i n =
  match n with
  | One _ -> 1
  | S m -> 1 + p2i m

let rec target m =
  match m with
  | Value (k, v) ->
      let cnt = p2i v in
      Value (k, v)
  | Node (a, l, r) -> Node (a, target l, target r)

let program m = if is_map m then spec (repr (target m)) else 0