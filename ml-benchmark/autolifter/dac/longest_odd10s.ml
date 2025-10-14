type list =
  | Cons of int * list
  | Nil

let is_even a = a = 2 * (a / 2)

let rec length x =
  match x with
  | Cons (_, t) -> length t + 1
  | Nil -> 0

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil -> y

let split xs =
  let rec f x n =
    if n < 1 then (Nil, x)
    else
      match x with
      | Cons (h, t) ->
        let res = f t (n - 1) in
        match res with
        | (a, b) -> (Cons (h, a), b)
      | _ -> (Nil, Nil)
  in
  f xs (length xs / 2)

let dac v =
  fun xs ->
    let rec run xs =
      match xs with
      | Nil -> xs
      | Cons (_, t) ->
        match t with
        | Nil -> xs
        | _ ->
          let sp = split xs in
          match sp with
          | (a, b) -> concat (run a) (run b)
    in
    v (run xs)

let inf = 100

let max a b = if a < b then b else a

let rec longest_odd10s_f s1 s2 len xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) ->
    let s1 = s2 && h = 1 in
    let s2 = h = 0 in
    let len = if s1 then len + 1 else if s2 then len else 0 in
    if is_even len then longest_odd10s_f s1 s2 len t
    else max len (longest_odd10s_f s1 s2 len t)

let longest_odd10s = longest_odd10s_f false false 0

let program = dac longest_odd10s