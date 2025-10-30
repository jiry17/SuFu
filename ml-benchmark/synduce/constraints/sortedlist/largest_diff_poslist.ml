type l = Elt of int | Cons of int * l

let is_sorted xs =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre <= x
    | Cons (h, t) -> pre <= h && aux h t
  in match xs with
    | Elt x -> true
    | Cons (h, t) -> aux h t

let rec is_pos xs =
  match xs with
  | Elt x -> x > 0
  | Cons (h, t) -> h > 0 && is_pos t

let max x y = if x < y then y else x
let min x y = if x > y then y else x

let spec xs =
  let rec f xs =
    match xs with
    | Elt x -> (0, x, x)
    | Cons (h, t) ->
        let res = f t in
        match res with
        | (r1, r2, r3) ->
            let newmax = max h r2 in
            let newmin = min h r3 in
            (max r1 (newmax - newmin), newmax, newmin)
  in
  match f xs with
  | (a, b, c) -> a

let rec last xs =
  match xs with
  | Elt a -> a
  | Cons (h, t) -> last t

val target: l -> l compress
let rec target xs =
  match xs with
  | Elt w -> xs
  | Cons (h, t) ->
      let aux = last t in
      Cons (h, t)

let program xs = if is_sorted xs && is_pos xs then spec (target xs) else 0