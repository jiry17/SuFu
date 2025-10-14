type list_t = Nil of unit | Cons of int * int * list_t
type slist = Snil of unit | Scons of int * slist

let rec repr xs =
  match xs with
  | Nil _ -> Snil ()
  | Cons (a, b, t) -> Scons (a, Scons (b, repr t))

let next_is_lt pre xs =
  match xs with
  | Nil _ -> true
  | Cons (a, _, _) -> pre > a

let rec is_sorted xs =
  match xs with
  | Nil _ -> true
  | Cons (a, b, t) ->
      (a > 0 && b > 0) && (a > b && (next_is_lt b t && is_sorted t))

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Snil _ -> (0, 0)
    | Scons (h, t) ->
        let res = f t in
        match res with
        | r1, r2 -> (max h r1, max r2 (min h r1))
  in
  let pair = f xs in
  match pair with
  | (_, r2) -> r2

let rec target c =
  match c with
  | Nil _ -> c
  | Cons (_, _, _) -> c

let program xs =
  if is_sorted xs then spec (repr (target xs)) else 0