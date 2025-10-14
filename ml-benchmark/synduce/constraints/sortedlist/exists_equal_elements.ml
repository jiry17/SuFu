type list_ = Elt of int | Cons of int * list_
type twolist = { l1: list_; l2: list_ }

let is_sorted =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre <= x
    | Cons (h, t) -> pre <= h && aux h t
  in
  fun xs ->
    match xs with
    | Elt _ -> true
    | Cons (h, t) -> aux h t

let is_sorted_pair p = is_sorted p.l1 && is_sorted p.l2

let find w =
  let rec f xs =
    match xs with
    | Elt x -> x = w
    | Cons (h, t) -> h = w || f t
  in
  f

let spec p =
  let rec f xs =
    match xs with
    | Elt a -> find a p.l1
    | Cons (h, t) -> find h p.l1 || f t
  in
  f p.l2

let rec target p =
  match p with
  | { l1; l2 } ->
      (match l1 with
       | Elt a ->
           (match l2 with
            | Elt _ -> p
            | Cons (h, t) ->
                if a > h then p
                else
                  let res = target { l1 = Elt a; l2 = t } in
                  { l1 = res.l1; l2 = Cons (h, res.l2) })
       | Cons (h, t) ->
           (match l2 with
            | Elt b ->
                if b > h then p
                else
                  let res = target { l1 = t; l2 = Elt b } in
                  { l1 = Cons (h, res.l1); l2 = res.l2 }
            | Cons (h2, t2) ->
                if h > h2 then
                  let res = target { l1 = t; l2 = l2 } in
                  { l1 = Cons (h, res.l1); l2 = res.l2 }
                else
                  let res = target { l1 = l1; l2 = t2 } in
                  { l1 = res.l1; l2 = Cons (h2, res.l2) }))

let program p =
  if is_sorted_pair p then spec (target p) else false