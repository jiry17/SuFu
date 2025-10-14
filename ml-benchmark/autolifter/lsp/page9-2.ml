type l = Cons of int * l | Nil of unit

let head x default =
  match x with
  | Cons (h, t) -> h
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

let fold_list f x w0 =
  let rec g x =
    match x with
    | Cons (h, t) -> f h (g t)
    | _ -> w0
  in
  g x

let rev x =
  let rec f x y =
    match x with
    | Cons (h, t) -> f t (Cons (h, y))
    | _ -> y
  in
  f x (Nil ())

let max x y = if x < y then y else x
let inf = 100

let lsp b x =
  let rec f x =
    match x with
    | Cons (h, t) ->
        let res = f t in
        match res with
        | (x2, ms) ->
            if b (Cons (h, x2)) then
              (Cons (h, x2), max ms (1 + length x2))
            else
              if b (Cons (h, Nil ())) then
                (Cons (h, Nil ()), max ms 1)
              else
                (Nil (), ms)
    | _ -> (Nil (), 0)
  in
  match f x with
  | (_, ms) -> ms

let min x y = if x < y then x else y
let minimum x = fold (fun h t -> min h t) x inf
let isval x =
  match x with
  | Cons (h, t) -> if h > minimum t then false else true
  | _ -> true

let run = lsp isval