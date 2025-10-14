type list_ = Elt of int | Cons of int * list_
type twolist = list_ * list_

let is_sorted =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre <= x
    | Cons (h, t) -> pre <= h && aux h t
  in
  fun xs ->
    match xs with
    | Elt x -> true
    | Cons (h, t) -> aux h t

let is_sorted_pair =
  fun p ->
    match p with
    | (l1, l2) -> is_sorted l1 && is_sorted l2

let find w =
  let rec f xs =
    match xs with
    | Elt x -> x = w
    | Cons (h, t) -> h = w || f t
  in
  f

let spec p =
  match p with
  | (l1, l2) ->
    let rec f xs =
      match xs with
      | Elt a -> find a l1
      | Cons (h, t) -> find h l1 || f t
    in
    f l2

let target =
  let rec f p =
    match p with
    | (Elt a, Elt b) -> p
    | (Elt a, Cons (h, t)) ->
      if a > h then p else
        let res = f (Elt a, t) in
        match res with
        | (r1, r2) -> (r1, Cons (h, r2))
    | (Cons (h, t), Elt b) ->
      if b > h then p else
        let res = f (t, Elt b) in
        match res with
        | (r1, r2) -> (Cons (h, r1), r2)
    | (Cons (h1, t1), Cons (h2, t2)) ->
      if h1 = h2 then p
      else if h1 > h2 then
        let res = f (t1, Cons (h2, t2)) in
        match res with
        | (r1, r2) -> (Cons (h1, r1), r2)
      else
        let res = f (Cons (h1, t1), t2) in
        match res with
        | (r1, r2) -> (r1, Cons (h2, r2))
  in
  f

let program p =
  if is_sorted_pair p then
    spec (target p)
  else
    false