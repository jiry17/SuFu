config SampleSize = 20

type list = Elt of int | Cons of int * list
type nlist = Line of list | NCons of list * nlist
type cnlist = Sglt of list | Cat of cnlist * cnlist

let rec cton c =
  let rec dec l c1 =
    match c1 with
    | Sglt x -> NCons (x, cton l)
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
  let rec res xs =
    match xs with
    | Line a ->
      let s = sum a in
      let ms = max s 0 in
      (ms, ms, ms, s)
    | NCons (h, t) ->
      let hsum = sum h in
      let r = res t in
      match r with
      | (r1, r2, r3, r4) ->
        (max (r1 + hsum) 0,
         max r2 (r1 + hsum),
         max (r4 + hsum) r3,
         r4 + hsum)
  in
  match res xs with
  | (a, b, c, _) -> (a, b, c)

val target: cnlist -> cnlist compress
let rec target c =
  match c with
  | Sglt x ->
    let info = sum x in
    c
  | Cat (l, r) -> Cat (target l, target r)

let program c = spec (cton (target c))