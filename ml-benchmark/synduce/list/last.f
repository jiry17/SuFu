Inductive List = elt Int | cons {Int, List};
Inductive CList = single Int | concat {CList, CList};

spec = fix (
  \f: List -> Int. \x: List.
  match x with
    elt a -> a
  | cons {h, t} -> f t
  end
);

cat = fix (
  \f: List -> List -> List. \a: List. \b: List.
  match a with
    elt a -> cons {a, b}
  | cons {h, t} -> cons {h, (f t b)}
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