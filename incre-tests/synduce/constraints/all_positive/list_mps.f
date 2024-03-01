Inductive CList = cnil Unit | single Int | concat {CList, CList};
Inductive List = nil Unit | cons {Int, List};

allpos = fix (
  \f: CList -> Bool. \c: CList.
  match c with
    cnil _ -> true
  | single w -> (> w 0)
  | concat {l, r} -> and (f l) (f r)
  end
);

cat = fix (\f: List->List->List. \x: List. \y: List.
  match x with
    cons {h, t} -> cons {h, f t y}
  | nil _ -> y
  end
);

repr = fix (
  \f: CList -> List. \cl: CList.
  match cl with
    cnil _ -> nil unit
  | single h -> cons {h, nil unit}
  | concat {l, r} -> cat (f l) (f r)
  end
);

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if < a b then b else a;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> max (+ (f t) h) 0
  end
);

target = fix (
  \f: CList -> Compress CList. \xs: CList.
  match xs with
    cnil _ -> cnil unit
  | single h -> single h
  | concat {l, r} -> concat {f l, f r}
  end
);

main = \xs: CList. if allpos xs then spec (repr (target xs)) else 0;