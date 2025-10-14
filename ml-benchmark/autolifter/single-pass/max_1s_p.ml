type lst = Cons of int * lst | Nil

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let max a b =
  if a < b then b else a

let rec max1s_with_pos pre i xs =
  match xs with
  | Nil ->
      let len = i - pre in
      (len, pre)
  | Cons (h, t) ->
      if h = 1 then
        max1s_with_pos pre (i + 1) t
      else
        let len = i - pre in
        let res = max1s_with_pos (i + 1) (i + 1) t in
        match res with
        | (rlen, rpre) ->
            if len >= rlen then (len, pre) else (rlen, rpre)

let program = single_pass max1s_with_pos