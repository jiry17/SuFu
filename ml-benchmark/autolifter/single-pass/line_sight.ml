type l =
  | Cons of int * l
  | Nil

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let rec line_sight_rec ma xs =
  match xs with
  | Nil -> true
  | Cons (h, t) ->
      match t with
      | Nil -> h >= ma
      | _ ->
          if h > ma then line_sight_rec h t else line_sight_rec ma t

let line_sight = line_sight_rec (0 - inf)

let program = single_pass line_sight