type list =
  | Elt of int
  | Cons of int * list

type nested_list =
  | Line of list
  | NCons of list * nested_list

type cnlist =
  | Sglt of list
  | Cat of cnlist * cnlist

let rec concat xs ys = 
  match xs with
  | Line a -> NCons (a, ys)
  | NCons (h, t) -> NCons (h, concat t ys)

let rec clist_to_nlist = function 
  | Sglt a -> Line a
  | Cat (x, y) -> concat (clist_to_nlist x) (clist_to_nlist y)

let rec sum = function
  | Elt x -> x
  | Cons (hd, tl) -> hd + sum tl

let max a b = if a > b then a else b

let get_fst = function 
  | (x, _) -> x

let rec spec = function 
  | Line a -> (max 0 (sum a), sum a)
  | NCons (hd, tl) -> 
    match spec tl with 
    | (mbs, csum) ->
      let line_sum = sum hd in
      (max (line_sum + mbs) 0, csum + line_sum)

val repr: cnlist -> cnlist compress 
let rec repr = function 
  | Sglt x -> let info = sum x in Sglt x 
  | Cat (l, r) -> Cat (repr l, repr r)

let prog xs = get_fst (spec (clist_to_nlist (repr xs)))