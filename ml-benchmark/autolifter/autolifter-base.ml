type l = Nil | Cons of int * l

let al_fold oplus e =
  let rec f xs =
    match xs with
    | Cons (h, t) -> oplus h (f t)
    | Nil -> e
  in
  f

let al_error = 100
let al_inf = 100

let al_plus a b = a + b
let al_minus a b = a - b
let al_times a b = a * b
let al_min a b = if a < b then a else b
let al_max a b = if a > b then a else b
let al_maximum = al_fold al_max (0 - al_inf)
let al_minimum = al_fold al_min al_inf
let al_sum = al_fold al_plus 0
let al_length = al_fold (fun a b -> b + 1) 0
let al_head xs = match xs with Nil -> al_inf | Cons (h, _) -> h
let al_inc a = a + 1
let al_dec a = a - 1
let al_neg a = 0 - a

let al_last =
  let rec f xs =
    match xs with
    | Nil -> al_error
    | Cons (h, t) ->
      (match t with
       | Nil -> h
       | Cons (_, _) -> f t)
  in
  f

let al_access pos xs =
  let len = al_length xs in
  let ind = if pos < 0 then pos + len else pos in
  if ind < 0 || ind >= len then al_error
  else
    let rec f i ys =
      match ys with
      | Cons (h, t) -> if i = 0 then h else f (i - 1) t
    in
    f ind xs

let al_count p =
  let rec f xs =
    match xs with
    | Nil -> 0
    | Cons (h, t) -> if p h then 1 + f t else f t
  in
  f

let al_take pos xs =
  let len = al_length xs in
  let ind = if pos < 0 then pos + len else pos in
  let rec f i ys =
    match ys with
    | Nil -> ys
    | Cons (h, t) -> if i < 0 then Nil else Cons (h, f (i - 1) t)
  in
  f ind xs

let al_drop pos xs =
  let len = al_length xs in
  let ind = if pos < 0 then pos + len else pos in
  let rec f i ys =
    match ys with
    | Nil -> ys
    | Cons (h, t) -> if i >= ind then Cons (h, f (i + 1) t) else f (i + 1) t
  in
  f 0 xs

let rec al_rev_aux res xs =
  match xs with
  | Nil -> res
  | Cons (h, t) -> al_rev_aux (Cons (h, res)) t

let al_rev xs = al_rev_aux Nil xs

let al_map op =
  let rec f xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (op h, f t)
  in
  f

let al_filter p =
  let rec f xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> if p h then Cons (h, f t) else f t
  in
  f

let al_zip op =
  let rec f xs ys =
    match xs with
    | Nil -> Nil
    | Cons (h1, t1) ->
      (match ys with
       | Nil -> Nil
       | Cons (h2, t2) -> Cons (op h1 h2, f t1 t2))
  in
  f

let al_concat xs ys =
  let rec f zs =
    match zs with
    | Nil -> ys
    | Cons (h, t) -> Cons (h, f t)
  in
  f xs

let al_sort =
  let rec f xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) ->
      let l = al_filter (fun a -> a < h) t in
      let r = al_filter (fun a -> h <= a) t in
      al_concat (f l) (Cons (h, f r))
  in
  f

let al_scanl oplus xs =
  match xs with
  | Nil -> xs
  | Cons (h, t) ->
    let rec recf pre xs =
      match xs with
      | Cons (h, t) ->
        let now = oplus pre h in
        Cons (now, recf now t)
      | Nil -> xs
    in
    Cons (h, recf h t)

let al_scanr oplus =
  let rec f xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) ->
      (match t with
       | Nil -> xs
       | Cons (_, _) ->
         let res = f t in
         Cons (oplus h (al_head res), res))
  in
  f

let al_isneg a = a < 0
let al_ispos a = a > 0
let al_iseven a = a = ((a / 2) * 2)
let al_isodd a = not (al_iseven a)
let one = 1
let none = -1