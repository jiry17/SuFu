Inductive Nat = zero Unit | succ Nat;

pred = \n: Nat.
  match n with
    zero _ -> n
  | succ m -> m
  end;

fib = fix (
  \f: Nat -> Int. \n: Nat.
  match n with
    zero _ -> 0
  | succ (zero _) -> 1
  | succ (succ m) ->
    + (f m) (f (succ m))
  end
);

repr = fix (
  \f: Nat -> Compress Nat. \n: Nat.
  match n with
    zero _ -> n
  | succ m -> succ (f m)
  end
);

main = \n: Nat. fib (repr n);