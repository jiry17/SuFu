type list =
  | Elt of int
  | Cons of int * list

let rec insert y =
  let rec f xs =
    match xs with
    | Elt x ->
        if y < x then Cons (y, Elt x) else Cons (x, Elt y)
    | Cons (h, t) ->
        if y < h then Cons (y, xs) else Cons (h, f t)
  in
  f

let rec sort =
  let rec f xs =
    match xs with
    | Elt x -> Elt x
    | Cons (h, t) ->
        let g = insert h in
        g (f t)
  in
  f

let rec spec =
  let rec f xs =
    match xs with
    | Elt x -> x
    | Cons (h, t) -> f t
  in
  f

let rec target =
  let rec f xs =
    match xs with
    | Elt x -> Elt x
    | Cons (h, t) -> Cons (h, f t)
  in
  f

let program xs = spec (sort (target xs))