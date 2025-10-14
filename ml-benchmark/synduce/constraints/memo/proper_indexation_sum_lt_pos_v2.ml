type idlist = Inil of unit | Icons of int * int * idlist
type lst = Nil of unit | Cons of int * lst

let rec length xs =
  match xs with
  | Inil _ -> 0
  | Icons (_, _, t) -> 1 + length t

let rec is_indexed xs =
  match xs with
  | Inil _ -> true
  | Icons (_, id, t) -> is_indexed t && id = length t

let rec repr m =
  match m with
  | Inil _ -> Nil ()
  | Icons (h, _, t) -> Cons (h, repr t)

let rec len xs =
  match xs with
  | Nil _ -> 0
  | Cons (_, t) -> 1 + len t

let max a b =
  if a > b then a else b

let rec spec xs =
  match xs with
  | Nil _ -> (0, 0)
  | Cons (h, t) ->
      let res = spec t in
      match res with
      | (r1, r2) ->
          let a =
            if h > r2 then
              max (r1 + h) 0
            else
              r1
          in
          let b = r2 + 1 in
          (a, b)

let rec target xs =
  match xs with
  | Inil _ -> xs
  | Icons (h, id, t) -> Icons (h, id, target t)

let program m =
  if is_indexed m then spec (repr (target m)) else (0, 0)