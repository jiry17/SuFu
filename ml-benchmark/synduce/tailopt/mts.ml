type list_ = Nil of unit | Cons of int * list_

let max a b = if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Nil _ -> (0, 0)
    | Cons (h, t) ->
        let r = f t in
        match r with
        | (r1, r2) ->
            let s = h + r1 in
            (s, max r2 s)
  in
  match f xs with
  | (_, b) -> b

let rec snoc xs w =
  match xs with
  | Nil _ -> Cons (w, Nil ())
  | Cons (h, t) -> Cons (h, snoc t w)

let repr =
  let rec f pre xs =
    match xs with
    | Nil _ -> pre
    | Cons (h, t) -> f (snoc pre h) t
  in
  fun xs -> f (Nil ()) xs

let program xs = spec (repr xs)