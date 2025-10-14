type list_ = Elt of int | Cons of int * list_
type clist = Single of int | Concat of clist * clist

let min x y = if x < y then x else y


let rec f xs =
  match xs with
  | Elt a -> (a, 1)
  | Cons (hd, tl) ->
      let result = f tl in
      match result with
      | (rm, rc) ->
          let new_min = min rm hd in
          let new_cnt = if hd < rm then 1 else rc + (if hd == rm then 1 else 0) in
          (new_min, new_cnt)

let spec xs =
  let p = f xs in
  match p with
  | (_, cnt) -> cnt

let rec cat xs ys =
  match xs with
  | Elt a -> Cons (a, ys)
  | Cons (a, b) -> Cons (a, cat b ys)

val repr: clist -> list_ compress
let rec repr xs =
  match xs with
  | Single a -> Elt a
  | Concat (a, b) ->
      let fa = repr a in
      let fb = repr b in
      cat fa fb

let program x = spec (repr x)