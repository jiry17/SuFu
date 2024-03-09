type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec spec l = 
  let (_, _, _, res) = f l in res 
and f = function
  | Nil -> 0, 0, 0, 0
  | Cons (hd, tl) ->
    let sum, mts, mps, mss = f tl in
    sum + hd, max mts (sum + hd), max (mps + hd) 0, max mss (max (mps + hd) 0)
  [@@ensures
    fun (sum, mts, mps, mss) ->
      mts >= 0
      && mps >= 0
      && mps >= sum
      && mts >= sum
      && mss >= 0
      && mss >= mts
      && mss >= sum
      && mss >= mps]
;;

let rec target = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (y, z) -> [%synt odot] (target y) (target z)
;;

let rec repr t = c t

and c = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;

assert (target = repr @@ spec)
