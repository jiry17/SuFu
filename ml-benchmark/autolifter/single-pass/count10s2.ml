type list_ = Cons of int * list_ | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let rec count10s2_aux s0 xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) ->
      let upd = if s0 && (h = 2) then 1 else 0 in
      let s0 =
        let b1 = h = 1 in
        let b2 = s0 && (h = 0) in
        b1 || b2
      in
      upd + count10s2_aux s0 t

let count10s2 xs = count10s2_aux false xs

let program = single_pass count10s2