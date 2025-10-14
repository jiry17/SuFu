config NonLinear = true 
config EnableDeepCoder = true
type list = Nil of unit | Cons of int * list
type clist = CNil of unit | Single of int | Concat of clist * clist

let max a b = if a < b then b else a

@Input val x: int 

let spec xs =
  let rec f xs =
    match xs with
    | Nil _ -> (0, 1)
    | Cons (hd, tl) ->
        let result = f tl in
        match result with
        | (r1, r2) -> ((r1 + (hd * r2)), (x * r2))
  in
  match f xs with
  | (v1, v2) -> v1

let rec cat xs ys =
  match xs with
  | Nil _ -> ys
  | Cons (a, b) -> Cons (a, cat b ys)

val repr: clist -> list compress
let rec repr xs =
  match xs with
  | CNil _ -> Nil ()
  | Single a -> Cons (a, Nil ())
  | Concat (a, b) -> cat (repr a) (repr b)

let program c = spec (repr c)