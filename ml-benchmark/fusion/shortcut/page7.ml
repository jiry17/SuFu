type list_ = Nil of unit | Cons of int * list_

let div x y =
  if y = 0 then 0 else x / y

let rec sum xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> h + sum t

let rec length xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> 1 + length t

let rec res xs =
  match xs with
  | Nil _ -> Nil ()
  | Cons (h, t) -> Cons (h, res t)

let program xs =
  let oup = res xs in
  div (sum oup) (length oup)