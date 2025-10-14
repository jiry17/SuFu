type nat = Z of unit | S of nat
type inat = Positive of nat | Negative of nat
type twoinats = TwoInats of inat * inat

type eint = Zero of unit | Sub1 of eint | Add1 of eint
type twoints = TwoInts of eint * eint

let rec nsum n =
  match n with
  | Z _ -> 0
  | S m ->
      let r = nsum m in
      1 + r

let itoint n =
  match n with
  | Negative m ->
      let s = nsum m in
      0 - (1 + s)
  | Positive m ->
      nsum m

let two_isum n =
  match n with
  | TwoInats (n1, n2) ->
      let a = itoint n1 in
      let b = itoint n2 in
      a + b

let rec target_eint e =
  match e with
  | Zero _ -> Zero ()
  | Add1 e' ->
      let r = target_eint e' in
      Add1 r
  | Sub1 e' ->
      let r = target_eint e' in
      Sub1 r

let target_twoints x =
  match x with
  | TwoInts (x1, x2) ->
      let y1 = target_eint x1 in
      let y2 = target_eint x2 in
      TwoInts (y1, y2)

let rec irepr e =
  match e with
  | Zero _ -> Positive (Z ())
  | Sub1 e' ->
      let res = irepr e' in
      (match res with
       | Positive n1 ->
           (match n1 with
            | Z _ -> Negative (Z ())
            | S n -> Positive n)
       | Negative n ->
           let n' = S n in
           Negative n')
  | Add1 e' ->
      let res = irepr e' in
      (match res with
       | Positive n ->
           let sn = S n in
           Positive sn
       | Negative n1 ->
           (match n1 with
            | Z _ -> Positive (Z ())
            | S n -> Negative n))

let repr x =
  match x with
  | TwoInts (x1, x2) ->
      let y1 = irepr x1 in
      let y2 = irepr x2 in
      TwoInats (y1, y2)

let program x =
  let tx = target_twoints x in
  let r = repr tx in
  two_isum r