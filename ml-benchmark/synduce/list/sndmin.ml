type ilist = Two of int * int | Cons of int * ilist
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
    | Two (a, b) -> (min a b, max a b)
    | Cons (hd, tl) ->
        let result = f tl in
        match result with
        | (r1, r2) ->
            let new_min = min hd r1 in
            let new_snd = min r2 (max hd r1) in
            (new_min, new_snd)
  in
  match f xs with
  | (_, s2) -> s2

let program x = spec (repr x)