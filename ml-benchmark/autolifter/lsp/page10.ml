type list_ = Cons of int * list_ | Nil

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
  f x Nil

let max x y = if x < y then y else x
let inf = 100

let lsp b x =
  let rec f x =
    match x with
    | Cons (h, t) ->
        let res = f t in
        let ms = snd res in
        let x1 = fst res in
        if b (Cons (h, x1)) then (Cons (h, x1), max ms (1 + length x1))
        else
          if b (Cons (h, Nil)) then (Cons (h, Nil), max ms 1)
          else (Nil, ms)
    | _ -> (Nil, 0)
  in
  snd (f x)

let min x y = if x < y then x else y
let minimum x = fold (fun h t -> min h t) x inf
let maximum x = fold (fun h t -> max h t) x (0 - inf)

let cond1 x =
  match x with
  | Cons (h, t) -> not (h > minimum t)
  | _ -> true

let gap = 0

let cond2 x =
  let ma = maximum x in
  let mi = minimum x in
  not ((mi + gap) < ma)

let isval x = (cond1 x) && (cond2 x)

let run = lsp isval