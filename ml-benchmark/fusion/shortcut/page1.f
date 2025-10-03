Inductive List = nil Unit | cons {Int, List};
Inductive BoolList = bnil Unit | bcons {Bool, BoolList};

map = \f: Int -> Bool. fix (
  \g: List -> Compress BoolList. \xs: List.
  match xs with
    nil _ -> bnil unit
  | cons {h, t} -> bcons {f h, g t}
  end
);

p = \x: Int. >= 0 x;

all = fix (
  \f: BoolList -> Bool. \xs: BoolList.
  match xs with
    bnil _ -> true
  | bcons {h, t} -> and h (f t)
  end
);

main = \xs: List. all (map p xs);