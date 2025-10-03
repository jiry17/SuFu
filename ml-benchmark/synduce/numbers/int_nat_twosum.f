Inductive Nat = z Unit | s Nat;
Inductive INat = positive Nat | negative Nat;
Inductive TwoInats = twoinats {INat, INat};

Inductive EInt = zero Unit | sub1 EInt | add1 EInt;
Inductive TwoInts = twoints {EInt, EInt};

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

two_isum = \n: TwoInats. 
  match n with
    twoinats {n1, n2} -> + (itoint n1) (itoint n2)
  end;

target_eint = fix (
  \f: EInt -> Compress EInt. \e: EInt.
  match e with
    zero _ -> zero unit
  | add1 e' -> add1 (f e')
  | sub1 e' -> sub1 (f e')
  end
);

target_twoints = \x: TwoInts.
  match x with
    twoints {x1, x2} -> twoints {target_eint x1, target_eint x2}
  end;

irepr = fix (
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

repr = \x: TwoInts.
  match x with
    twoints {x1, x2} -> twoinats {irepr x1, irepr x2}
  end;

main = \x: TwoInts. two_isum (repr (target_twoints x));
