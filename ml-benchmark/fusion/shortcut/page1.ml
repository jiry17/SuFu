type list =
  | Nil
  | Cons of int * list

type bool_list =
  | BNil
  | BCons of bool * bool_list

let rec map f xs =
  match xs with
  | Nil -> BNil
  | Cons (h, t) ->
      let bh = f h in
      let bt = map f t in
      BCons (bh, bt)

let p x = x >= 0

let rec all xs =
  match xs with
  | BNil -> true
  | BCons (h, t) -> h && all t

let program xs = all (map p xs)