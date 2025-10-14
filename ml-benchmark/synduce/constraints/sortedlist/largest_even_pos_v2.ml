type lst =
  | Elt of int
  | Cons of int * lst

let is_sorted =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre <= x
    | Cons (h, t) -> pre <= h && aux h t
  in
  fun xs ->
    match xs with
    | Elt x -> true
    | Cons (h, t) -> aux h t

let max x y = if x < y then y else x

let mod2 x = x - 2 * (x / 2)

let rec spec xs =
  match xs with
  | Elt x -> if mod2 x = 0 && x > 0 then x else 0
  | Cons (h, t) ->
      if mod2 h = 0 && h > 0 then max h (spec t) else spec t

let rec target xs =
  match xs with
  | Elt w -> xs
  | Cons (h, t) ->
      if h <= 0 then xs else Cons (h, target t)

let program xs = if is_sorted xs then spec (target xs) else 0