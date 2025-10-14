type nat = Z of unit | S of nat
type inat = Positive of nat | Negative of nat
type eint = Zero of unit | Sub1 of eint | Add1 of eint

let rec nsum n =
  match n with
  | Z _ -> 0
  | S m -> 1 + nsum m

let itoint n =
  match n with
  | Negative m -> -(1 + nsum m)
  | Positive m -> nsum m

let rec repr e =
  match e with
  | Zero _ -> Positive (Z ())
  | Sub1 e' ->
      let res = repr e' in
      match res with
      | Positive n ->
          (match n with
           | Z _ -> Negative (Z ())
           | S n' -> Positive n')
      | Negative n -> Negative (S n)
  | Add1 e' ->
      let res = repr e' in
      match res with
      | Positive n -> Positive (S n)
      | Negative n ->
          (match n with
           | Z _ -> Positive (Z ())
           | S n' -> Negative n')

let rec target e =
  match e with
  | Zero u -> Zero u
  | Add1 e' -> Add1 (target e')
  | Sub1 e' -> Sub1 (target e')

let program x = itoint (repr (target x))