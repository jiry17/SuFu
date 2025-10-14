type list =
  | Elt of int
  | Cons of int * list

type nlist =
  | Line of list
  | Ncons of list * nlist

type cnlist =
  | Sglt of list
  | Cat of cnlist * cnlist

let cton =
  let rec f c =
    let dec =
      let rec g l c2 =
        match c2 with
        | Sglt x -> Ncons (x, f l)
        | Cat (x, y) -> g (Cat (y, l)) x
      in
      g
    in
    match c with
    | Sglt x -> Line x
    | Cat (x, y) -> dec y x
  in
  f

let max a b = if a < b then b else a
let min a b = if a < b then a else b

let range =
  let rec f xs =
    match xs with
    | Elt w -> (w, w)
    | Cons (h, t) ->
        let res = f t in
        match res with
        | (a, b) -> (min h a, max h b)
  in
  f

let spec =
  let rec f xs =
    match xs with
    | Line a -> range a
    | Ncons (h, t) ->
        let rh = range h in
        let res = f t in
        match rh with
        | (rh1, rh2) ->
            match res with
            | (r1, r2) -> (min rh1 r1, max rh2 r2)
  in
  f

let target =
  let rec f c =
    match c with
    | Sglt x ->
        let info = range x in
        c
    | Cat (l, r) -> Cat (f l, f r)
  in
  f

let program c = spec (cton (target c))