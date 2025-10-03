Inductive List = elt Int | cons {Int, List};
Inductive CList = single Int | concat {CList, CList};

max = \x: Int. \y: Int. if (> x y) then x else y;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt a -> a
  | cons {hd, tl} -> max hd (f tl)
  end
);

cat = fix (
  \f: List -> List -> List. \xs: List. \ys: List.
  match xs with
    elt a -> cons {a, ys}
  | cons {a, b} -> cons {a, f b ys}
  end
);

repr = fix (
  \f: CList -> Compress List. \xs: CList.
  match xs with
    single a -> elt a
  | concat {a, b} -> cat (f a) (f b)
  end
);

main = \x: CList. spec (repr x);