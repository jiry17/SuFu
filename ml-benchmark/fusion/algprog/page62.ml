type l = Nil | Cons of int * l

let incre x =
  let w = x / 10 in
  (w, x - (10 * w))

let double n =
  let rec current xs =
    match xs with
    | Nil -> (0, Nil)
    | Cons (h, t) ->
        let subres = current t in
        match subres with
        | (sr1, sr2) ->
            let a = h + (h + sr1) in
            let info = incre a in
            match info with
            | (i1, i2) -> (i1, Cons (i2, sr2))
  in
  match n with
  | (n1, n2) ->
      let info = current n2 in
      match info with
      | (i1, i2) -> (n1 + (n1 + i1), i2)

let dnum = 10

let rec ratio_f n =
  if n = 0 then 1 else 2 * ratio_f (n - 1)

let ratio = ratio_f dnum

let head xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) -> h

let multi xs =
  let rec f n =
    if n <= 0 then (0, xs)
    else
      let res = f (n - 1) in
      double res
  in
  f dnum

let round xs =
  let res = multi xs in
  match res with
  | (r1, r2) ->
      if head r2 >= 5 then r1 + 1 else r1

let rec repr xs =
  match xs with
  | Nil -> Nil
  | Cons (h, t) -> Cons (h, repr t)

let rec valid xs =
  match xs with
  | Nil -> true
  | Cons (h, t) ->
      if h < 0 || h >= 10 then false else valid t

let program xs =
  if valid xs then round (repr xs) else 0