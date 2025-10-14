type lst =
  | Cons of int * lst
  | Nil

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let max a b = if a < b then b else a

let rec largest_peak_f cmo xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) ->
    let cmo = if h > 0 then cmo + h else 0 in
    max cmo (largest_peak_f cmo t)

let largest_peak = largest_peak_f 0

let program = single_pass largest_peak