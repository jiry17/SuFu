type nat = Zero of unit | Succ of nat

let pred n =
  match n with
  | Zero _ -> n
  | Succ m -> m

let rec fib n =
  match n with
  | Zero _ -> 0
  | Succ m ->
      match m with
      | Zero _ -> 1
      | Succ m2 -> fib m2 + fib (Succ m2)

let rec repr n =
  match n with
  | Zero _ -> n
  | Succ m -> Succ (repr m)

let program n = fib (repr n)