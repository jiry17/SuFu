Config NonLinear = true;

Inductive List = nil Unit | cons {Int, List};
Inductive CList = cnil Unit | single Int | concat {CList, CList};

max = \a: Int. \b: Int. if < a b then b else a;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 1
  | cons {hd, tl} -> * hd (f tl)
  end
);

cat = fix (
  \f: List -> List -> List. \xs: List. \ys: List.
  match xs with
    nil _ -> ys
  | cons {a, b} -> cons {a, f b ys}
  end
);

repr = fix (
  \f: CList -> Compress List. \xs: CList.
  match xs with
    cnil _ -> nil unit
  | single a -> cons {a, nil unit}
  | concat {a, b} -> cat (f a) (f b)
  end
);

main = \x: CList. spec (repr x);