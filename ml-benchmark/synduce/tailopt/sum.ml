type l = Nil of unit | Cons of int * l

let rec spec xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> h + spec t

let rec snoc xs w =
  match xs with
  | Nil _ -> Cons (w, Nil ())
  | Cons (h, t) -> Cons (h, snoc t w)

let rec repr_aux pre xs =
  match xs with
  | Nil _ -> pre
  | Cons (h, t) -> repr_aux (snoc pre h) t

let repr xs = repr_aux (Nil ()) xs

let program xs = spec (repr xs)