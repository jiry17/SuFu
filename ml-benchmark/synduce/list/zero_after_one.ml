type blist = Nil | Cons of bool * blist
type clist = Emp | Single of bool | Concat of clist * clist

let const_true = true
let const_false = false
let op_and = fun x -> fun y -> x && y
let op_or = fun x -> fun y -> x || y

let rec cat_list xs ys =
  match xs with
  | Nil -> ys
  | Cons (hd, tl) -> Cons (hd, cat_list tl ys)

let rec repr xs =
  match xs with
  | Emp -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (a, b) -> cat_list (repr a) (repr b)

let spec xs =
  let rec f xs =
    match xs with
    | Nil -> (false, false, false)
    | Cons (hd, tl) ->
      let result = f tl in
      match result with
      | (r1, r2, r3) ->
        let new_seen1 = r1 || hd in
        let new_res = r2 || (r1 && (not hd)) in
        let new_aux = r3 || (not hd) in
        (new_seen1, new_res, new_aux)
  in
  match f xs with
  | (_, r2, _) -> r2

let program xs = spec (repr xs)