Config SampleSize = 5;
Config NonLinear = true;

Inductive List = nil Unit | cons {Int, List};
Inductive NList = single List | ncons {List, NList};

head = \xs: NList.
  match xs with
    single w -> w
  | ncons {h, t} -> h
  end;

map = \f: List -> Int. fix (
  \g: NList -> List. \xs: NList.
  match xs with
    single w -> cons {f w, nil unit}
  | ncons {h, t} -> cons {f h, g t}
  end
);

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

product = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 1
  | cons {h, t} -> * h (f t)
  end
);

tails = fix (
  \f: List -> Compress NList. \xs: List.
  match xs with
    nil _ -> single xs
  | cons {h, t} -> ncons {xs, f t}
  end
);

main = \xs: List. sum (map product (tails xs));
