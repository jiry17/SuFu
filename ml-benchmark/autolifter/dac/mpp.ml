let rec length x =
  match x with
  | [] -> 0
  | _ :: t -> length t + 1

let rec concat x y =
  match x with
  | [] -> y
  | h :: t -> h :: concat t y

let split xs =
  let rec f x n =
    if n < 1 then ([], x)
    else
      match x with
      | h :: t ->
          match f t (n - 1) with
          | (l1, l2) -> (h :: l1, l2)
      | _ -> ([], [])
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run xs =
    match xs with
    | [] -> xs
    | _ ->
        match xs with
        | h :: t ->
            match t with
            | [] -> xs
            | _ ->
                match split xs with
                | (a, b) -> concat (run a) (run b)
        | _ -> xs
  in
  v (run xs)

let max a b = if a < b then b else a

let rec mpp_aux pre l =
  match l with
  | [] -> pre
  | h :: t -> max pre (mpp_aux (h * pre) t)

let mpp l = mpp_aux 1 l

let program = dac mpp