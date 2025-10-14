type l = Nil | Cons of int * l

let map f =
  let rec g xs =
    match xs with
    | Nil -> Nil
    | Cons (h, t) -> Cons (f h, g t)
  in
  g

let rec sum xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) -> h + sum t

let square x = x * x

let upto n =
  let rec f now =
    if now > n then Nil
    else Cons (now, f (now + 1))
  in
  f 1

let program n = sum (map square (upto n))