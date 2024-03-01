Inductive List = nil Unit | cons {Int, List};
Inductive NList = single List | ncons {List, NList};

max = \a: Int. \b: Int. if < a b then b else a;

head = \xs: NList.
  match xs with
    single a -> a
  | ncons {h, t} -> h
  end;

maximum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> max h (f t)
  end
);

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

map = \f: List -> Int. fix (
  \g: NList -> List. \xs: NList.
  match xs with
    single a -> cons {f a, nil unit}
  | ncons {h, t} -> cons {f h, g t}
  end
);

collect = fix (
  \f: List -> Compress NList. \xs: List.
  match xs with
    nil _ -> single (nil unit)
  | cons {h, t} -> ncons {xs, f t}
  end
);

main = \xs: List. maximum (map sum (collect xs));