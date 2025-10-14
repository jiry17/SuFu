type list_ = Elt of int | Cons of int * list_
type idlist = IElt of int | ICons of int * int * idlist

let is_sorted =
  let rec aux pre xs =
    match xs with
    | IElt x -> pre < x
    | ICons (h, _, t) -> (pre < h) && aux h t
  in
  fun xs ->
    match xs with
    | IElt _ -> true
    | ICons (h, _, t) -> aux h t

let rec len xs =
  match xs with
  | IElt _ -> 1
  | ICons (_, _, t) -> 1 + len t

let rec len_raw xs =
  match xs with
  | Elt _ -> 1
  | Cons (_, t) -> 1 + len_raw t

let rec is_indexed xs =
  match xs with
  | IElt _ -> true
  | ICons (_, id, t) -> (id = len xs) && is_indexed t

let rec add_index xs =
  match xs with
  | Elt a -> IElt a
  | Cons (h, t) -> ICons (h, len_raw xs, add_index t)

let rec drop_index xs =
  match xs with
  | IElt a -> Elt a
  | ICons (h, _, t) -> Cons (h, drop_index t)

let w = 0

let rec spec xs =
  match xs with
  | Elt x -> if x < w then 1 else 0
  | Cons (h, t) -> spec t + (if h < w then 1 else -1)

let rec target xs =
  match xs with
  | IElt _ -> xs
  | ICons (h, idx, t) -> if h < w then xs else ICons (h, idx, target t)

let program xs =
  let inp = add_index xs in
  if (is_sorted inp) && (is_indexed inp) then spec (drop_index (target inp)) else 0