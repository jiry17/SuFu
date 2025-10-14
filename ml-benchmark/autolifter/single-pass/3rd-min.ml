type tlist = Cons of { h: int; t: tlist } | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons {h; t} -> Cons {h; t = run t}
  in
  fun xs -> v (run xs)

let inf = 100

let min a b = if a < b then a else b
let max a b = if a > b then a else b

let trdmin xs =
  let rec f xs =
    match xs with
    | Nil _ -> (inf, inf, inf)
    | Cons {h; t} ->
        let res = f t in
        match res with
        | (r1, r2, r3) ->
            (min r1 (max r2 h), min r2 (max r3 h), min r3 h)
  in
  match f xs with
  | (a, _, _) -> a

let program = single_pass trdmin