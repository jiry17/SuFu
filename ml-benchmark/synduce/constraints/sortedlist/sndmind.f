Inductive CList = ctwo {Int, Int} | concat {CList, CList};
Inductive List = two {Int, Int} | cons {Int, List};

cat = fix (
  \f: List -> List -> List. \a: List. \b: List.
  match a with
    two {x, y} -> cons {x, cons {y, b}}
  | cons {h, t} -> cons {h, (f t b)}
  end
);

repr = fix (
  \f: CList -> List. \xs: CList.
  match xs with
    ctwo {x, y} -> two {x, y}
  | concat {a, b} -> cat (f a) (f b)
  end
);

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if < a b then b else a;

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    two {x, y} -> {min x y, max x y}
  | cons {h, t} ->
    let res = f t in
      {min res.1 h, min res.2 (max res.1 h)}
  end
) xs).2;

is_sorted =
  let aux = fix (
    \f: Int -> List -> Bool. \pre: Int. \xs: List.
    match xs with
      two {x, y} -> and (> pre x) (> x y)
    | cons {h, t} -> and (> pre h) (f h t)
    end
  ) in \xs: List.
  match xs with
    two {x, y} -> > x y
  | cons {h, t} -> aux h t
  end;

target = fix (
  \f: CList -> Compress CList. \c: CList.
  match c with
    ctwo {a, b} -> c
  | concat {l, r} -> concat {l, f r} /*Avoid the recursion of r*/
  end
);

main = \c: CList.
  if is_sorted (repr c) then spec (repr (target c)) else 0;