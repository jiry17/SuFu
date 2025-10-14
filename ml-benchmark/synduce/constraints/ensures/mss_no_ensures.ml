type l = Nil | Cons of int * l
type cl = CNil | Single of int | Concat of cl * cl

let max a b = if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Nil -> (0, 0, 0, 0)
    | Cons (hd, tl) ->
        let result = f tl in
        match result with
        | (r1, r2, r3, r4) ->
            let new_sum = hd + r1 in
            let new_mts = max r2 (hd + r1) in
            let new_mps = max 0 (hd + r3) in
            let new_mss = max r4 (max 0 (r3 + hd)) in
            (new_sum, new_mts, new_mps, new_mss)
  in
  match f xs with
  | (_, _, _, r4) -> r4

let rec cat xs ys =
  match xs with
  | Nil -> ys
  | Cons (a, b) -> Cons (a, cat b ys)

let rec repr xs =
  match xs with
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)