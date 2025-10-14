type list =
  | Cons of int * list
  | Nil

let div a b =
  if b = 0 then 0 else a / b

let rec length x =
  match x with
  | Cons (_, t) ->
      let r = length t in
      r + 1
  | Nil -> 0

let rec concat x y =
  match x with
  | Cons (h, t) ->
      let rest = concat t y in
      Cons (h, rest)
  | Nil -> y

let split xs =
  let rec f x n =
    if n < 1 then
      (Nil, x)
    else
      match x with
      | Cons (h, t) ->
          let res = f t (n - 1) in
          match res with
          | (a, b) -> (Cons (h, a), b)
      | Nil -> (Nil, Nil)
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (_, t) ->
        match t with
        | Nil -> xs
        | _ ->
            let sp = split xs in
            match sp with
            | (a, b) ->
                let ra = run a in
                let rb = run b in
                concat ra rb
  in
  v (run xs)

let average xs =
  let len = length xs in
  let sum =
    let rec f ys =
      match ys with
      | Nil -> 0
      | Cons (h, t) ->
          let rt = f t in
          h + rt
    in
    f xs
  in
  div sum len

let program = dac average