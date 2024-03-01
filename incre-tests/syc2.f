Inductive IInt = cint Int;
Inductive List = nil Unit | cons {IInt, List};
Inductive Llist = cnil Unit | ccons {List, Llist};
Inductive Pair = pii {Int, Int};

cint2int = lambda x: IInt.
match x with
  cint a -> a
end;

cmax = lambda x: Int. lambda y: Int. if (<= x y) then y else x;

lhead = \xs: Llist. match xs with
  cnil _ -> nil unit
| ccons {h, t} -> h
end;

mapcons = fix (lambda f: Llist -> IInt -> Llist.
lambda ll: Llist.
lambda h: IInt.
match ll with
  cnil _ -> cnil unit
| ccons {ch, ct} -> ccons {cons {h, ch}, f ct h}
end);



prefixes = fix (lambda f: List -> Llist.
lambda l: List.
match l with
   nil _ -> cnil unit
| cons {h, t} -> ccons {l, f t}
end
);

cappend =
fix (lambda f: Llist -> Llist -> Llist. lambda l: Llist. lambda m: Llist.
match l with
  ccons {h, t} -> ccons {h, (f t m)}
| cnil _ -> m
end
);

sum = fix (lambda f: List -> Int.
lambda l: List.
match l with
  nil _ -> 0
| cons {h, t} -> + (cint2int h) (f t)
end);


func0 = fix (lambda f: Llist -> Int.
lambda ll: Llist.
match ll with
  cnil _ -> 0
| ccons {ch, ct} -> cmax (sum ch) (f ct)
end
);

func1 = fix (lambda f: List -> Compress Llist.
lambda l: List.
match l with
  nil _ -> let var6 = 0 in cnil unit
| cons {h, t} -> cappend (prefixes l) (f t)
end
);

func2 = lambda var4: List.
let var5 = (func1 var4) in
let var4 = (nil unit) in
(func0 var5);
