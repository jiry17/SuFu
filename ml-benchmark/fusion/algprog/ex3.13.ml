type list_ = Nil | Cons of int * list_
type plist = PNil | PCons of int * int * plist

let rec tri xs =
  match xs with
  | Nil -> PNil
  | Cons (h, t) ->
    let tail =
      (let rec g ys =
        match ys with
        | PNil -> PNil
        | PCons (n, h2, t2) -> PCons (n + 1, h2, g t2)
      in g) (tri t)
    in
    PCons (0, h, tail)

let rec tsum xs =
  match xs with
  | PNil -> 0
  | PCons (n, h, t) -> n * h + tsum t

let program xs = tsum (tri xs)