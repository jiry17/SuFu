type list =
  | Nil
  | Cons of int * list

type clist =
  | CNil
  | Single of int
  | Concat of clist * clist

let min a b = if a > b then b else a

let spec xs =
  let rec f x =
    match x with
    | Nil -> (0, 0, true)
    | Cons (h, t) ->
      match f t with
      | (cnt, min_cnt, bal) ->
        let new_cnt = if h > 0 then cnt + 1 else cnt - 1 in
        (new_cnt, min min_cnt new_cnt, bal && new_cnt >= 0)
  in
  match f xs with
  | (_, _, result) -> result

let cat =
  let rec f a b =
    match a with
    | Nil -> b
    | Cons (h, t) -> Cons (h, f t b)
  in
  f

val repr: clist -> list compress
let repr =
  let rec f xs =
    match xs with
    | CNil -> Nil
    | Single x -> Cons (x, Nil)
    | Concat (a, b) -> cat (f a) (f b)
  in
  f

let prog x = spec (repr x)