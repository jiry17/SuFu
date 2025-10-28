type tlist = Elt of int | Cons of int * tlist
type mlist = Ielt of int | Icons of int * int * mlist

let rec repr m =
  match m with
  | Ielt a -> Elt a
  | Icons (h, _, t) ->
      let r = repr t in
      Cons (h, r)

let rec sum xs =
  match xs with
  | Elt a -> a
  | Cons (h, t) ->
      let st = sum t in
      h + st

let rec is_memo m =
  match m with
  | Ielt _ -> true
  | Icons (_, s, t) ->
      let eq =
        let r = repr m in
        let v = sum r in
        s == v
      in
      let recres = is_memo t in
      eq && recres

let max a b = if a < b then b else a

let rec spec xs =
  match xs with
  | Elt x -> x
  | Cons (_, t) ->
      let ft = spec t in
      let su = sum xs in
      max ft su

val target: mlist -> mlist compress
let rec target m =
  match m with
  | Ielt _ -> m
  | Icons (h, s, t) ->
      let ft = target t in
      Icons (h, s, ft)

let program m =
  if is_memo m then
    let r = target m in
    let rr = repr r in
    spec rr
  else
    0