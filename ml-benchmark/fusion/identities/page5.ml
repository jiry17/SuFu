type list_ = Nil of unit | Cons of int * list_
type nlist = Single of list_ | Ncons of list_ * nlist

let head xs =
  match xs with
  | Single w -> w
  | Ncons (h, t) -> h

let map f =
  let rec g xs =
    match xs with
    | Single w -> Cons (f w, Nil ())
    | Ncons (h, t) -> Cons (f h, g t)
  in
  g

let rec sum xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> h + sum t

let rec product xs =
  match xs with
  | Nil _ -> 1
  | Cons (h, t) -> h * product t

let rec tails xs =
  match xs with
  | Nil _ -> Single xs
  | Cons (h, t) -> Ncons (xs, tails t)

let program xs = sum (map product (tails xs))