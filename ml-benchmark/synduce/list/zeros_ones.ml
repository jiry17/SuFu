type list_ = Elt of bool | Cons of bool * list_
type clist = Single of bool | Concat of clist * clist

let rec cat_list xs ys =
  match xs with
  | Elt a -> Cons (a, ys)
  | Cons (a, b) -> Cons (a, cat_list b ys)

let rec repr xs =
  match xs with
  | Single a -> Elt a
  | Concat (a, b) -> cat_list (repr a) (repr b)

let spec xs =
  let rec f xs =
    match xs with
    | Elt a -> (a, true, a)
    | Cons (hd, tl) ->
      let result = f tl in
      match result with
      | (r1, r2, r3) ->
        let new_an = r1 && hd in
        let new_bn = r2 && (r1 || not hd) in
        (new_an, new_bn, r3)
  in
  match f xs with
  | (_, b, _) -> b

let program xs = spec (repr xs)