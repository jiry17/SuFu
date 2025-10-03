Inductive Nat = z Unit | s Nat;
Inductive INat = positive Nat | negative Nat;

nsum = fix (
  \f: Nat -> Int. \n: Nat.
  match n with
    z _ -> 0
  | s m -> + 1 (f m)
  end
);

itoint = \n: INat.
  match n with
    negative m -> - 0 (+ 1 (nsum m))
  | positive m -> nsum m
  end;

Inductive EInt = zero Unit | sub1 EInt | add1 EInt;

repr = fix (
  \f: EInt -> INat. \e: EInt.
  match e with
    zero _ -> positive (z unit)
  | sub1 e' ->
    let res = f e' in
    match res with
      positive (z _) -> negative (z unit)
    | positive (s n) -> positive n
    | negative n -> negative (s n)
    end
  | add1 e' ->
    let res = f e' in
    match res with
      positive n -> positive (s n)
    | negative (z _) -> positive (z unit)
    | negative (s n) -> negative n
    end
  end
);

target = fix (
  \f: EInt -> Compress EInt. \e: EInt.
  match e with
    zero _ -> zero unit
  | add1 e' -> add1 (f e')
  | sub1 e' -> sub1 (f e')
  end
);

main = \x: EInt. itoint (repr (target x));
