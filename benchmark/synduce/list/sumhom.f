Inductive CList = cnil Unit | single Int | concat {CList, CList};
Inductive List = nil Unit | cons {Int, List};

cat = fix (\f: List->List->List. \x: List. \y: List.
  match x with
    cons {h, t} -> cons {h, f t y}
  | nil _ -> y
  end
);

repr = fix (
  \f: CList -> Compress List. \cl: CList.
  match cl with
    cnil _ -> nil unit
  | single h -> cons {h, nil unit}
  | concat {l, r} -> cat (f l) (f r)
  end
);

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

@Start main = \cl: CList. spec (repr cl);