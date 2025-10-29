type list_ = Elt of int | Cons of int * list_
type clist = Single of int | Concat of int * clist * clist

let rec cat a b =
  match a with
  | Elt w -> Cons (w, b)
  | Cons (h, t) -> Cons (h, cat t b)

let rec repr c =
  match c with
  | Single w -> Elt w
  | Concat (_, l, r) -> cat (repr l) (repr r)

let max a b =
  if a < b then b else a

let min a b =
  if a < b then a else b

let rec lmax c =
  match c with
  | Single w -> w
  | Concat (_, l, r) -> max (lmax l) (lmax r)

let rec lmin c =
  match c with
  | Single w -> w
  | Concat (_, l, r) -> min (lmin l) (lmin r)

let rec is_parti c =
  match c with
  | Single _ -> true
  | Concat (w, l, r) -> (lmax l) < w && w < (lmin r) && (is_parti l && is_parti r)

let rec spec xs =
  match xs with
  | Elt w -> (w, max w 0, max w 0, max w 0)
  | Cons (h, t) ->
      let res = spec t in
      match res with
      | (a1, a2, a3, a4) ->
          let s1 = a1 + h in
          let s2 = max a2 s1 in
          let s3 = max (a3 + h) 0 in
          let s4 = max a4 (a3 + h) in
          (s1, s2, s3, s4)

let rec sum c =
  match c with
  | Single a -> a
  | Concat (_, l, r) -> sum l + sum r

val target: clist -> clist compress
let rec target xs =
  match xs with
  | Single _ -> xs
  | Concat (w, l, r) ->
      if w < 0 then
        let s = sum l in
        Concat (w, l, target r)
      else
        Concat (w, target l, target r)

let rec insert w xs =
  match xs with
  | Elt a ->
      if w < a then Cons (w, Elt a) else Cons (a, Elt w)
  | Cons (h, t) ->
      if w < h then Cons (w, xs) else Cons (h, insert w t)

let rec sort xs =
  match xs with
  | Elt _ -> xs
  | Cons (h, t) -> insert h (sort t)

let access x =
  match x with
  | Elt w -> (w, x)
  | Cons (h, t) -> (h, t)

let rec fill c xs =
  match c with
  | Single _ ->
      let info = access xs in (
      match info with
      | (i1, i2) -> (Single i1, i2)
      )
  | Concat (_, l, r) ->
      let lres = fill l xs in
      match lres with
      | (lcl, lxs_tail) ->
          let info = access lxs_tail in (
          match info with
          | (i1, i2) ->
            let rres = fill r i2 in (
              match rres with
              | (rcl, rxs_tail) -> (Concat (i1, lcl, rcl), rxs_tail)
            )
          )

let rec flatten c =
  match c with
  | Single w -> Elt w
  | Concat (w, l, r) -> cat (flatten l) (Cons (w, flatten r))

let gen c =
  let xs = sort (flatten c) in
  match fill c xs with
  | (cl, _) -> cl

let program xs =
  let inp = gen xs in
  if is_parti inp then spec (repr (target inp)) else (0, 0, 0, 0)