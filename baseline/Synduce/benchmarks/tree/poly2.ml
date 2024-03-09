type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree

let spec x t =
  let rec main t = 
    let w, _ = f (0, 1) t in w
  and f s = function
    | Nil -> s
    | Node (a, l, r) ->
      let y, z = f s l in
      f (a + (x * y), x * z) r
  in
  main t
;;

let target x t =
  let rec h = function
    | Nil -> [%synt s0]
    | Node (a, l, r) -> [%synt join] x a (h l) (h r)
  in
  h t
;;

assert (target = spec)
