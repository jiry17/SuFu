type list_ = Nil of unit | Cons of int * list_
type nat = Z of unit | S of nat
type alist = ANil of unit | ACons of int * nat * alist

let is_unique =
  let key_differ key =
    let rec f xs =
      match xs with
      | ANil _ -> true
      | ACons (h, _, t) -> (not (h == key)) && f t
    in
    f
  in
  let rec f xs =
    match xs with
    | ANil _ -> true
    | ACons (h, _, t) -> (key_differ h t) && f t
  in
  f

let spec w =
  let rec f xs =
    match xs with
    | Nil _ -> 0
    | Cons (h, t) -> if h == w then 1 + f t else f t
  in
  f

let repr =
  let repeat w suf =
    let rec f n =
      match n with
      | Z _ -> suf
      | S m -> Cons (w, f m)
    in
    f
  in
  let rec f xs =
    match xs with
    | ANil _ -> Nil ()
    | ACons (h, n, t) ->
      let rt = f t in
      let rep = repeat h rt in
      rep n
  in
  f

let n2i =
  let rec f n =
    match n with
    | Z _ -> 0
    | S m -> 1 + f m
  in
  f

val target: int -> alist -> alist compress
let target w =
  let rec f xs =
    match xs with
    | ANil _ -> ANil ()
    | ACons (h, n, t) ->
      if h == w then
        let num = n2i n in
        ACons (h, n, t)
      else
        ACons (h, n, f t)
  in
  f

let program w xs =
  if is_unique xs then
    let r = target w xs in
    let l = repr r in
    let sp = spec w in
    sp l
  else
    0