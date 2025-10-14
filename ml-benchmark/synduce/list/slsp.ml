type tlist = Elt of int | Cons of int * tlist
type cnlist = Sglt of int | Cat of cnlist * int * cnlist

let max x y = if x > y then x else y

let rec cat_list xs ys =
  match xs with
  | Elt a -> Cons (a, ys)
  | Cons (hd, tl) -> Cons (hd, cat_list tl ys)

let rec repr xs =
  match xs with
  | Sglt a -> Elt a
  | Cat (a, b, c) -> cat_list (repr a) (repr c)

let spec xs =
  let rec f xs =
    match xs with
    | Elt a -> (max 0 a, a >= 0)
    | Cons (hd, tl) ->
        let result = f tl in
        match result with
        | (s, b) ->
            let new_cond = b && hd >= 0 in
            if new_cond then (hd + s, new_cond) else (s, new_cond)
  in
  match f xs with
  | (s, b) -> s

let program x = spec (repr x)