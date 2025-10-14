type list_ = Cons of int * list_ | Nil

let rec length x =
  match x with
  | Cons (_, t) -> 1 + length t
  | Nil -> 0

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
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
          | (l, r) -> (Cons (h, l), r)
      | _ -> (Nil, Nil)
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

let inf = 100

let rec min xs =
  match xs with
  | Nil -> inf
  | Cons (h, t) ->
      let res = min t in
      if res < h then res else h

let program = dac min