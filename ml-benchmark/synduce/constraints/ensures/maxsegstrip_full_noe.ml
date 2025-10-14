let sampleSize = 20

type llist = Elt of int | Cons of int * llist
type nlist = Line of llist | Ncons of llist * nlist
type cnlist = Sglt of llist | Cat of cnlist * cnlist

let rec cton c =
  let rec dec l c =
    match c with
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
  let rec res_f xs =
    match xs with
    | Line a ->
        let s = sum a in
        let ms = max s 0 in
        (ms, ms, ms, s)
    | Ncons (h, t) ->
        let hsum = sum h in
        let res = res_f t in
        match res with
        | (r1, r2, r3, r4) ->
            let a1 = max (r1 + hsum) 0 in
            let a2 = max r2 (r1 + hsum) in
            let a3 = max (r4 + hsum) r3 in
            let a4 = r4 + hsum in
            (a1, a2, a3, a4)
  in
  let res = res_f xs in
  match res with
  | (r1, r2, r3, _) -> (r1, r2, r3)

let rec target c =
  let rec list_repr xs =
    match xs with
    | Elt x -> Elt x
    | Cons (h, t) -> Cons (h, list_repr t)
  in
  match c with
  | Sglt x -> Sglt (list_repr x)
  | Cat (l, r) -> Cat (target l, target r)

let program c = spec (cton (target c))