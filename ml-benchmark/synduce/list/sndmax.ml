type llist = Two of int * int | Cons of int * llist
type clist = Ctwo of int * int | Concat of clist * clist

let max a b = if a < b then b else a
let min a b = if a > b then b else a

let rec cat_list xs ys =
  match xs with
  | Two (a, b) -> Cons (a, Cons (b, ys))
  | Cons (hd, tl) -> Cons (hd, cat_list tl ys)

let rec repr xs =
  match xs with
  | Ctwo (a, b) -> Two (a, b)
  | Concat (a, b) -> cat_list (repr a) (repr b)

let spec xs =
  let rec f xs =
    match xs with
    | Two (a, b) -> (max a b, min a b)
    | Cons (hd, tl) ->
      let result = f tl in
      match result with
      | (r1, r2) ->
        let new_max = max hd r1 in
        let new_snd = max r2 (min hd r1) in
        (new_max, new_snd)
  in
  match f xs with
  | (_, b) -> b

let program x = spec (repr x)