type t = Cons of int * t | Nil of unit

let head x default =
  match x with
  | Cons (h, _) -> h
  | _ -> default

let fold f x w0 =
  let rec g x =
    match x with
    | Cons (h, t) -> f h (g t)
    | _ -> w0
  in
  g x

let length x = fold (fun a b -> b + 1) x 0
let sum x = fold (fun a b -> a + b) x 0
let lim = 10
let minimum x = fold (fun a b -> if a < b then a else b) x 0

let rec pushback x a =
  match x with
  | Cons (h, t) -> Cons (h, pushback t a)
  | Nil _ -> Cons (a, Nil ())

let max a b = if a < b then b else a

let tail x =
  match x with
  | Cons (_, t) -> t
  | Nil _ -> x

let lsp pred x =
  let rec f l suf lpre res =
    let len = length lpre in
    if (len = 0) || (pred lpre) then
      match l with
      | Cons (h, t) -> f t suf (pushback lpre h) (max res len)
      | Nil _ -> max res len
    else
      match suf with
      | Cons (_, t) -> f l t (tail lpre) res
      | Nil _ -> res
  in
  f x x (Nil ()) 0

let isvalid x = sum x < lim
let run x = if minimum x < 0 then 0 else lsp isvalid x