type list = Nil of unit | Cons of int * list
type zipper = Zip of list * list

let rec concat xs ys =
  match xs with
  | Nil _ -> ys
  | Cons (h, t) -> Cons (h, concat t ys)

let rec rev xs =
  match xs with
  | Nil _ -> Nil ()
  | Cons (h, t) -> concat (rev t) (Cons (h, Nil ()))

let rec sum xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> h + sum t

let repr z =
  match z with
  | Zip (l, r) -> concat (rev l) r

val target: zipper -> zipper compress
let target z =
  match z with
  | Zip (l, r) ->
      let a = sum l in
      let b = sum r in
      Zip (l, r)

let program z = sum (repr (target z))