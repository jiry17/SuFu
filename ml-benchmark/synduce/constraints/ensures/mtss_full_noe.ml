type list_ = Elt of int | Cons of int * list_
type nlist = Line of list_ | Ncons of list_ * nlist
type cnlist = Sglt of list_ | Cat of cnlist * cnlist

let rec cton c =
  let rec dec l c2 =
    match c2 with
    | Sglt x -> Ncons (x, cton l)
    | Cat (x, y) -> dec (Cat (y, l)) x
  in
  match c with
  | Sglt x -> Line x
  | Cat (x, y) -> dec y x

let rec sum xs =
  match xs with
  | Elt x -> x
  | Cons (h, t) -> h + sum t

let max a b = if a < b then b else a

let spec xs =
  let rec f ys =
    match ys with
    | Line a -> (max 0 (sum a), sum a)
    | Ncons (h, t) ->
        let hsum = sum h in
        match f t with
        | (u1, u2) -> (max (u1 + hsum) 0, u2 + hsum)
  in
  match f xs with
  | (u, _) -> u

let rec target c =
  let rec tsum xs =
    match xs with
    | Elt x -> Elt x
    | Cons (h, t) -> Cons (h, tsum t)
  in
  match c with
  | Sglt x -> Sglt (tsum x)
  | Cat (l, r) -> Cat (target l, target r)

let program c = spec (cton (target c))