type list = Elt of int | Cons of int * list
type nlist = Line of list | Ncons of list * nlist

let max a b = if a < b then b else a

let rec lmax xs =
  match xs with
  | Elt x -> x
  | Cons (h, t) -> max h (lmax t)

let min a b = if a < b then a else b

let rec lmin xs =
  match xs with
  | Elt x -> x
  | Cons (h, t) -> min h (lmin t)

let is_sorted xs =
  let rec aux pre xs =
    match xs with
    | Line x -> pre <= lmax x
    | Ncons (h, t) -> (pre <= lmax h) && aux (lmax h) t
  in
  match xs with
  | Line _ -> true
  | Ncons (h, t) -> aux (lmax h) t

let rec interval xs =
  match xs with
  | Elt x -> (x, x)
  | Cons (h, t) ->
      let res = interval t in
      match res with
      | (a, b) -> (min a h, max b h)

let rec spec_helper xs =
  match xs with
  | Line x ->
      let res = interval x in
      match res with
      | (a, b) -> (a, b, true)
  | Ncons (h, t) ->
      let info = interval h in
      let res = spec_helper t in
      match info with
      | (i1, i2) ->
          match res with
          | (r1, r2, r3) ->
              (min i1 r1, max i2 r2, r3 && ((r1 <= i1) && (r2 >= i2)))

let spec xs =
  let res = spec_helper xs in
  match res with
  | (_, _, b) -> b

let rec target xs =
  match xs with
  | Line x ->
      let info = interval x in
      xs
  | Ncons (h, t) ->
      let mi = lmin h in
      Ncons (h, target t)

let program xs =
  if is_sorted xs then spec (target xs) else false