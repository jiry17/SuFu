type list_ = Elt of int | Cons of int * list_
type clist = Single of int | Concat of int * clist * clist

let rec cat a b =
  match a with
  | Elt w -> Cons (w, b)
  | Cons (h, t) -> Cons (h, cat t b)

let rec repr c =
  match c with
  | Single w -> Elt w
  | Concat (w, l, r) -> cat (repr l) (repr r)

let max a b = if a < b then b else a
let min a b = if a < b then a else b

let is_parti =
  let rec lmax c =
    match c with
    | Single w -> w
    | Concat (w, l, r) -> max (lmax l) (lmax r)
  in
  let rec lmin c =
    match c with
    | Single w -> w
    | Concat (w, l, r) -> min (lmin l) (lmin r)
  in
  let rec f c =
    match c with
    | Single _ -> true
    | Concat (w, l, r) -> (lmax l) < w && w < (lmin r) && f l && f r
  in
  f

let spec xs =
  let rec f xs =
    match xs with
    | Elt w -> (max 0 w, w >= 0)
    | Cons (h, t) ->
      let res = f t in
      match res with
      | (res1, res2) ->
        let cond = (h >= 0) && res2 in
        ((if cond then res1 + h else res1), cond)
  in
  let tmp = f xs in
  match tmp with
  | (v, _) -> v

let rec target xs =
  match xs with
  | Single _ -> xs
  | Concat (w, l, r) ->
    if w <= 0 then Concat (w, l, target r) else Concat (w, target l, target r)

let program xs =
  let inp = xs in
  if is_parti inp then spec (repr (target inp)) else 0