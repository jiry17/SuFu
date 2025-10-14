type list_ = Nil of unit | Cons of int * list_

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let mtp xs =
  let rec f l =
    match l with
    | Nil _ -> (1, 1)
    | Cons (h, t) ->
      let res = f t in
      let tot = h * (snd res) in
      (max (fst res) tot, tot)
  in
  fst (f xs)

let program = single_pass mtp