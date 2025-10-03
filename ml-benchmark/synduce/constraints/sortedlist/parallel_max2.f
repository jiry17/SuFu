Inductive List = elt Int | cons {Int, List};
Inductive CList = single Int | concat {CList, CList};

cat = fix (
  \f: List -> List -> List. \a: List. \b: List.
  match a with
    elt x -> cons {x, b}
  | cons {h, t} -> cons {h, (f t b)}
  end
);

repr = fix (
  \f: CList -> List. \xs: CList.
  match xs with
    single x -> elt x
  | concat {a, b} -> cat (f a) (f b)
  end
);

max = \a: Int. \b: Int. if < a b then b else a;
lmax = fix (
  \f: CList -> Int. \xs: CList.
  match xs with
    single x -> x
  | concat {x, y} -> max (f x) (f y)
  end
);

min = \a: Int. \b: Int. if < a b then a else b;
lmin = fix (
  \f: CList -> Int. \xs: CList.
  match xs with
    single x -> x
  | concat {x, y} -> min (f x) (f y)
  end
);

is_part = fix (
  \f: CList -> Bool. \c: CList.
  match c with
    single w -> true
  | concat {x, y} ->
    and (> (lmin x) (lmax y)) (and (f x) (f y))
  end
);

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> x
  | cons {h, t} -> max h (f t)
  end
);

target = fix (
  \f: CList -> Compress CList. \c: CList.
  match c with
    single w -> c
  | concat {x, y} -> concat {f x, y} /*Avoid the recursion of y*/
  end
);

main = \xs: CList. if is_part xs then spec (repr (target xs)) else 0;