let div2 x = x / 2

let rec from a b =
  if a > b then []
  else a :: from (a + 1) b

let rec sum xs =
  match xs with
  | [] -> 0
  | h :: t -> h + sum t

let program a b =
  if a <= b then sum (from a b) else 0