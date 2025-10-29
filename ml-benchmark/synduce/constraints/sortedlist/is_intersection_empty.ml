type list_ = Elt of int | Cons of int * list_

let is_sorted xs =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre >= x
    | Cons (h, t) -> pre >= h && aux h t
  in
  match xs with
  | Elt x -> true
  | Cons (h, t) -> aux h t

let is_sorted_pair p =
  match p with
  | (l1, l2) -> is_sorted l1 && is_sorted l2

let rec find w xs =
  match xs with
  | Elt x -> x == w
  | Cons (h, t) -> h == w || find w t

let spec p =
  match p with
  | (l1, l2) ->
      let rec f xs =
        match xs with
        | Elt a -> find a l1
        | Cons (h, t) -> find h l1 || f t
      in
      f l2

val target: (list_ * list_) -> (list_ * list_) compress
let target p =
  let rec aux w xs =
    match xs with
    | Elt b -> (w, xs)
    | Cons (h, t) ->
        if w > h then (w, xs)
        else
          let res = aux w t in
          match res with
          | (_, l2) -> (w, Cons (h, l2))
  in
  let rec f p =
    match p with
    | (l1, l2) ->
        match l1 with
        | Elt a ->
            let res = aux a l2 in (
              match res with
              | (_, l2p) -> (Elt a, l2p)
            )
        | Cons (h, t) ->
            let res1 = f (t, l2) in
            let tmp = aux h l2 in (
              match res1 with
              | (l1p, l2p) -> (Cons (h, l1p), l2p)
            )
  in
  f p

let program p =
  if is_sorted_pair p then spec (target p) else false