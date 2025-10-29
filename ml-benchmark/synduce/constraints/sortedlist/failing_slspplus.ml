type lst =
  | Elt of int
  | Cons of int * lst

type nlist =
  | Line of lst
  | Ncons of lst * nlist

type clist =
  | Sglt of lst
  | Cat of clist * int * clist

let c2n =
  let rec aux pre c =
    match c with
    | Sglt a -> Ncons (a, pre)
    | Cat (l, _, r) -> aux (aux pre r) l
  in
  let rec f c =
    match c with
    | Sglt a -> Line a
    | Cat (l, _, r) -> aux (f r) l
  in
  f

let rec lsum xs =
  match xs with
  | Elt x -> x
  | Cons (h, t) -> h + lsum t

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let sorted =
  let rec lmin c =
    match c with
    | Sglt a -> lsum a
    | Cat (l, _, r) -> min (lmin l) (lmin r)
  in
  let rec lmax c =
    match c with
    | Sglt a -> lsum a
    | Cat (l, _, r) -> max (lmax l) (lmax r)
  in
  let rec f c =
    match c with
    | Sglt _ -> true
    | Cat (l, piv, r) ->
      (lmax l < piv) && (piv < lmin r) && (f l) && (f r)
  in
  f

let spec xs =
  let rec f xs =
    match xs with
    | Line a -> (max 0 (lsum a), lsum a >= 0)
    | Ncons (h, t) ->
      let res = f t in
      let line_sum = lsum h in
      match res with
      | (res1, res2) ->
        let new1 =
          if res2 && line_sum >= 0 then res1 + line_sum else res1
        in
        (new1, res2 && line_sum >= 0)
  in
  let s = f xs in
  match s with
  | (v, _) -> v

val target: clist -> clist compress
let target =
  let rec list_repr xs =
    match xs with
    | Elt _ -> xs
    | Cons (h, t) -> Cons (h, list_repr t)
  in
  let rec f c =
    match c with
    | Sglt x -> Sglt (list_repr x)
    | Cat (l, piv, r) ->
      if piv <= 0 then Cat (l, piv, f r)
      else Cat (f l, piv, f r)
  in
  f

let program c = if sorted c then spec (c2n (target c)) else 0