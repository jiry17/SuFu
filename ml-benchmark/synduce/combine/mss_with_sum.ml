type list =
  | Elt of int
  | Cons of int * list

type idx_list =
  | IElt of int
  | ICons of int * int * idx_list

let max a b = if a > b then a else b

let rec sum = function
  | IElt x -> x
  | ICons (hd, idx, tl) -> hd + sum tl

let rec is_memo_sum = function
  | IElt x -> true
  | ICons (hd, idx, tl) -> idx == sum tl && is_memo_sum tl

let rec hsum = function
  | Elt x -> x
  | Cons (hd, tl) -> hd + hsum tl

let rec mss = function
  | Elt x -> if x > 0 then (x, x, x) else (0, 0, 0)
  | Cons (hd, tl) ->
    match mss tl with
    | (mts_tl, mps_tl, mss_tl) ->
      let sum_tl = hsum tl in
      let new_mps = max (mps_tl + hd) 0 in
      (max mts_tl sum_tl, new_mps, max new_mps mss_tl)

val drop_sum_list: idx_list -> list compress
let rec drop_sum_list = function
  | IElt x -> Elt x
  | ICons (hd, ids, tl) -> Cons (hd, drop_sum_list tl)

let prog xs = mss (drop_sum_list xs)
