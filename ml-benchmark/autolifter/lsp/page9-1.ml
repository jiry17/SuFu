type list = Cons of int * list | Nil

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
      match res with
      | (x1, ms) ->
        if b (Cons (h, x1)) then (Cons (h, x1), max ms (1 + length x1))
        else
          match b (Cons (h, Nil)) with
          | true -> (Cons (h, Nil), max ms 1)
          | false -> (Nil, ms)
    | _ -> (Nil, 0)
  in
  match f x with
  | (_, ms) -> ms

let issorted x =
  let rec f l =
    match l with
    | Cons (h, t) ->
      let res = f t in
      match res with
      | (bflag, m) -> if h > m then (false, h) else (bflag, h)
    | Nil -> (true, inf)
  in
  match f x with
  | (bflag, _) -> bflag

let run = lsp issorted