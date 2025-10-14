type tlist = Nil of unit | Cons of int * tlist
type nlist = Nnil of unit | Ncons of tlist * nlist

let inf = 100

let map f =
  let rec g xs =
    match xs with
    | Nnil _ -> Nil ()
    | Ncons (h, t) ->
      let v1 = f h in
      let v2 = g t in
      Cons (v1, v2)
  in
  g

let max a b = if a < b then b else a
let min a b = if a < b then a else b

let rec minimum xs =
  match xs with
  | Nil _ -> inf
  | Cons (h, t) ->
    let v = minimum t in
    min h v

let rec maximum xs =
  match xs with
  | Nil _ -> 0 - inf
  | Cons (h, t) ->
    let v = maximum t in
    max h v

let program xs =
  let m = map maximum in
  let ys = m xs in
  minimum ys