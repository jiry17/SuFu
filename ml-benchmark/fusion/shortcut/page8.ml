type list_ = Nil of unit | Cons of int * list_
type nlist = NNil of unit | NCons of list_ * nlist

let rec length xs =
  match xs with
  | Nil _ -> 0
  | Cons (_, t) -> 1 + length t

let rec sum xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> h + sum t

let append w =
  let rec f a =
    match a with
    | Nil _ -> Cons (w, Nil ())
    | Cons (h, t) -> Cons (h, f t)
  in
  f

let rec cat a b =
  match a with
  | Nil _ -> b
  | Cons (h, t) -> Cons (h, cat t b)

let rec concat xs =
  match xs with
  | NNil _ -> Nil ()
  | NCons (h, t) -> cat h (concat t)

let safe p l n =
  let m = 1 + length l in
  let rec f xs i =
    match xs with
    | Nil _ -> true
    | Cons (j, t) ->
      if j = n || (i + j = n + m) || (i - j = m - n) then false
      else f t (i + 1)
  in
  f p 1

let queens n =
  let rec f m =
    if m = 0 then NCons (Nil (), NNil ())
    else
      let subres = f (m - 1) in
      let rec enum sols choice =
        match sols with
        | NNil _ ->
          if choice = n then NNil ()
          else enum subres (choice + 1)
        | NCons (sol, remain) ->
          let tailres = enum remain choice in
          if safe sol sol choice then
            NCons ((append choice) sol, tailres)
          else tailres
      in
      enum subres 1
  in
  f n

let program n =
  if n > 0 then queens n else NNil ()