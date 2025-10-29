@Input val w: int

type list = Nil of unit | Cons of int * list
type nat = Z of unit | S of nat

let is_unique =
  let key_differ key =
    let rec f xs =
      match xs with
      | Nil _ -> true
      | Cons (h, t) -> (not (h == key)) && f t
    in
    f
  in
  let rec f xs =
    match xs with
    | Nil _ -> true
    | Cons (h, t) -> (key_differ h t) && f t
  in
  f

let spec =
  let rec f xs =
    match xs with
    | Nil _ -> 0
    | Cons (h, t) -> if h == w then h + f t else f t
  in
  f

val target: list -> list compress
let target =
  let rec f xs =
    match xs with
    | Nil _ -> Nil ()
    | Cons (h, t) -> if h == w then Cons (h, t) else Cons (h, f t)
  in
  f

let program xs = if is_unique xs then spec (target xs) else 0