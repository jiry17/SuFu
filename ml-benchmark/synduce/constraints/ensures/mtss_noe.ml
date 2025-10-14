type list' = Elt of int | Cons of int * list'
type nlist = Line of list' | Ncons of list' * nlist
type cnlist = Sglt of list' | Cat of cnlist * cnlist

let rec cton c =
  match c with
  | Sglt x -> Line x
  | Cat (x, y) -> dec y x
and dec l c =
  match c with
  | Sglt x -> Ncons (x, cton l)
  | Cat (x, y) -> dec (Cat (y, l)) x

let rec sum xs =
  match xs with
  | Elt x -> x
  | Cons (h, t) -> h + sum t

let max a b = if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Line a -> (max 0 (sum a), sum a)
    | Ncons (h, t) ->
      let hsum = sum h in
      let tres = f t in
      match tres with
      | (a1, a2) -> (max (a1 + hsum) 0, a2 + hsum)
  in
  let res = f xs in
  match res with
  | (v1, v2) -> v1

let rec target c =
  match c with
  | Sglt x ->
    let info = sum x in
    c
  | Cat (l, r) -> Cat (target l, target r)

let program c = spec (cton (target c))