type list_ = Cons of int * list_ | Nil

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let msp =
  let rec f pre l =
    match l with
    | Nil -> pre
    | Cons (h, t) -> max pre (f (h * pre) t)
  in
  let mpp = f 1 in
  let rec g xs =
    match xs with
    | Nil -> 1
    | Cons (h, t) -> max (mpp xs) (g t)
  in
  g

let program = single_pass msp