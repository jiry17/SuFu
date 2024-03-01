Inductive Nat = zero Unit | succ Nat;
Inductive IInt = cint Int;
Inductive List = cons {IInt, List} | nil Unit;

nat_to_int = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);

append =
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List.
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> m
end
);


snoc =
fix (lambda f: List -> IInt -> List. lambda l: List. lambda m: IInt.
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> cons {m, nil unit}
end
);

length =
fix (lambda f: List -> Nat.
lambda l: List.
match l with
  cons {h, t} -> succ (f t)
| nil _ -> zero unit
end
);

rev = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> snoc (f t) h
| nil _ -> nil unit
end
);

func0 = fix (lambda f: List -> Compress List.
lambda l: List.
match l with
  cons {h, t} -> snoc (f t) h
| nil _ -> nil unit
end
);

func1 = lambda var1: List.
let var2 = func0 var1 in
let var1 = unit in rev var2;
