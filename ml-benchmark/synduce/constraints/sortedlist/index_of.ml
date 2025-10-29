@Input val key: int
type lst = Elt of int | Cons of int * lst

let is_sorted =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre <= x
    | Cons (h, t) -> (pre <= h) && aux h t
  in
  fun xs ->
    match xs with
    | Elt x -> true
    | Cons (h, t) -> aux h t

let rec spec xs =
  match xs with
  | Elt w -> if w == key then 1 else 0
  | Cons (h, t) ->
      let res = spec t in
      if key == h then 1 else if res == 0 then 0 else 1 + res

val target: lst -> lst compress
let rec target xs =
  match xs with
  | Elt w -> xs
  | Cons (h, t) ->
      if h >= key then xs else Cons (h, target t)

let program xs = if is_sorted xs then spec (target xs) else 0