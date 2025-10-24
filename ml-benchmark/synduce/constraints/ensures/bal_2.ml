config EnableDeepCoder = true
let SampleIntMin = -1
let SampleIntMax = 1
let SampleSize = 20
let ComposeNum = 2

type list_ = Nil of unit | Cons of int * list_
type clist = Cnil of unit | Single of int | Concat of clist * clist

let min a b = if a > b then b else a

let spec xs =
  let rec f x =
    match x with
    | Nil _ -> (0, 0, true)
    | Cons (h, t) ->
      let result = f t in
      match result with
      | (cnt, min_cnt, bal) ->
        let new_cnt = if h > 0 then cnt + 1 else cnt - 1 in
        (new_cnt, min min_cnt new_cnt, bal && new_cnt >= 0)
  in
  match f xs with
  | (_, _, b) -> b

let rec cat a b =
  match a with
  | Nil _ -> b
  | Cons (h, t) -> Cons (h, cat t b)

val repr: clist -> list_ compress
let rec repr xs =
  match xs with
  | Cnil _ -> Nil ()
  | Single x -> Cons (x, Nil ())
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)