Inductive List = nil Unit | cons {Int, List};
Inductive Zipper = zip {List, List};

concat = fix (
  \f: List -> List -> List. \xs: List. \ys: List.
  match xs with
    nil _ -> ys
  | cons {h, t} -> cons {h, f t ys}
  end
);

rev = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> concat (f t) (cons {h, nil unit})
  end
);

list_repr = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> cons {h, f t}
  end
);

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

repr = \z: Zipper.
  match z with
    zip {l, r} -> concat (rev l) (list_repr r)
  end;

main = \z: Zipper. sum (repr z);
