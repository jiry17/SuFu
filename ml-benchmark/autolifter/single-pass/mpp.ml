type lst = Nil | Cons of int * lst

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let rec mpp_f pre l =
  match l with
  | Nil -> pre
  | Cons (h, t) -> max pre (mpp_f (h * pre) t)

let mpp l = mpp_f 1 l

let program = single_pass mpp