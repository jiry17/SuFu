type l = Nil of unit | Cons of int * l

let tri op =
  let rec f xs =
    match xs with
    | Nil _ -> Nil ()
    | Cons (h, t) ->
      let tail =
        (let rec g ys =
           match ys with
           | Nil _ -> Nil ()
           | Cons (h2, t2) -> Cons (op h2, g t2)
         in g) (f t)
      in
      Cons (h, tail)
  in
  f

let w = read_int ()

let op x = x * w

let sum =
  let rec f xs =
    match xs with
    | Nil _ -> 0
    | Cons (h, t) -> h + f t
  in
  f

let program xs = sum (tri op xs)