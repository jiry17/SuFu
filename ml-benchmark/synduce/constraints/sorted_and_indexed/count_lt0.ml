type list_ = Elt of int | Cons of int * list_
type idlist = Ielt of int | Icons of int * int * idlist

let is_sorted =
  let rec aux pre xs =
    match xs with
    | Ielt x -> pre < x
    | Icons (h, _, t) -> pre < h && aux h t
  in
  fun xs ->
    match xs with
    | Ielt x -> true
    | Icons (h, _, t) -> aux h t

let rec len xs =
  match xs with
  | Ielt _ -> 1
  | Icons (_, _, t) -> 1 + len t

let rec len_raw xs =
  match xs with
  | Elt _ -> 1
  | Cons (_, t) -> 1 + len_raw t

let rec is_indexed xs =
  match xs with
  | Ielt _ -> true
  | Icons (_, id, t) -> id = len xs && is_indexed t

let rec add_index xs =
  match xs with
  | Elt a -> Ielt a
  | Cons (h, t) -> Icons (h, len_raw xs, add_index t)

let rec drop_index xs =
  match xs with
  | Ielt a -> Elt a
  | Icons (h, _, t) -> Cons (h, drop_index t)

let rec spec xs =
  match xs with
  | Elt x -> if x < 0 then 1 else 0
  | Cons (h, t) -> spec t + (if h < 0 then 1 else -1)

let rec target xs =
  match xs with
  | Ielt x -> xs
  | Icons (h, idx, t) -> if h < 0 then xs else Icons (h, idx, target t)

let program inp =
  if is_sorted inp && is_indexed inp then spec (drop_index (target inp)) else 0