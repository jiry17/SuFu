type list = Nil | Cons of int * list

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) ->
      let r = run t in
      Cons (h, r)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let rec mmm_aux p np z xs =
  match xs with
  | Nil -> max p (max np z)
  | Cons (h, t) ->
    let p1 = max z np + h in
    let np1 = max z p - h in
    let z1 = max p np in
    mmm_aux p1 np1 z1 t

let mmm = mmm_aux 0 0 0

let rec expected_aux p np z xs =
  match xs with
  | Nil -> (p, np, z)
  | Cons (h, t) ->
    let p1 = max z np + h in
    let np1 = max z p - h in
    let z1 = max p np in
    expected_aux p1 np1 z1 t

let expected = expected_aux 0 0 0

let program = single_pass mmm