type lst = Elt of int * int | Cons of int * int * lst

let head xs =
  match xs with
  | Elt (a, b) -> (a, b)
  | Cons (a, b, t) -> (a, b)

let rec sorted xs =
  match xs with
  | Elt (a, b) -> a < b
  | Cons (a, b, t) ->
      let h = head t in
      match h with
      | (a2, b2) -> (a < b) && ((a < a2) && (sorted t))

let inter a b =
  let rec f xs =
    match xs with
    | Elt (c, d) -> (not (b < c)) && (not (a > d))
    | Cons (c, d, t) -> (f t) || ((not (b < c)) && (not (a > d)))
  in
  f

let spec xs =
  let rec f xs =
    match xs with
    | Elt (a, b) -> (false, a, b)
    | Cons (a, b, t) ->
        let res = f t in
        match res with
        | (r1, r2, r3) -> ((r1 || (inter a b t)), a, b)
  in
  let r = f xs in
  match r with
  | (r1, r2, r3) -> r1

val target: lst -> lst compress
let rec target xs =
  match xs with
  | Elt (_, _) -> xs
  | Cons (a, b, t) -> Cons (a, b, (target t))

let program xs =
  if sorted xs then spec (target xs) else false