Inductive List = nil Unit | cons {Int, List};
Inductive NList = nnil Unit | ncons {List, NList};
inf = 100;

map = \f: List -> Int. fix (
  \g: NList -> Compress List. \xs: NList.
  match xs with
    nnil _ -> nil unit
  | ncons {h, t} -> cons {f h, g t}
  end
);

max = \a: Int. \b: Int. if < a b then b else a;
min = \a: Int. \b: Int. if < a b then a else b;
minimum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> inf
  | cons {h, t} -> min h (f t)
  end
);

maximum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> - 0 inf
  | cons {h, t} -> max h (f t)
  end
);

main = \xs: NList. minimum (map maximum xs);