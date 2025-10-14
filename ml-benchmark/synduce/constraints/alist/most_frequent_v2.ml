type list_ = Elt of int | Cons of int * list_
type pos = One of unit | S of pos
type alist = AElt of int * pos | ACons of int * pos * alist

let is_unique =
  let key_differ key =
    let rec f xs =
      match xs with
      | AElt (h, _) -> not (h = key)
      | ACons (h, _, t) -> (not (h = key)) && f t
    in
    f
  in
  let rec f xs =
    match xs with
    | AElt _ -> true
    | ACons (h, _, t) -> (key_differ h t) && f t
  in
  f

let max a b = if a < b then b else a

let count w =
  let rec f xs =
    match xs with
    | Elt h -> if h = w then 1 else 0
    | Cons (h, t) -> (if h = w then 1 else 0) + f t
  in
  f

let spec xs =
  let rec f xs =
    match xs with
    | Elt h -> (1, h)
    | Cons (h, t) ->
      let res = f t in
      match res with
      | (c1, _) ->
        let c = count h xs in
        if c > c1 then (c, h) else res
  in
  let res = f xs in
  match res with
  | (_, h) -> h

let repr =
  let repeat w suf =
    let rec f n =
      match n with
      | One _ -> Cons (w, suf)
      | S m -> Cons (w, f m)
    in
    f
  in
  let dup w =
    let rec f n =
      match n with
      | One _ -> Elt w
      | S m -> Cons (w, f m)
    in
    f
  in
  let rec f xs =
    match xs with
    | AElt (h, n) -> dup h n
    | ACons (h, n, t) -> repeat h (f t) n
  in
  f

let rec p2i n =
  match n with
  | One _ -> 1
  | S m -> 1 + p2i m

let rec target xs =
  match xs with
  | AElt (h, n) ->
    let num = p2i n in
    xs
  | ACons (h, n, t) ->
    let num = p2i n in
    ACons (h, n, target t)

let program xs =
  if is_unique xs then spec (repr (target xs)) else 0