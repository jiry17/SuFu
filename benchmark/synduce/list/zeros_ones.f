Inductive List = elt Bool | cons {Bool, List};
Inductive CList = single Bool | concat {CList, CList};

cat_list = fix (
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
  | concat {a, b} -> cat_list (f a) (f b)
  end
);

spec = \xs: List. (fix (
  \f: List -> {Bool, Bool, Bool}. \xs: List.
  match xs with
    elt a -> {a, true, a}
  | cons {hd, tl} -> 
    let result = f tl in
    let new_an = and result.1 hd in
    let new_bn = and result.2 (or result.1 (not hd)) in
    {new_an, new_bn, result.3}
  end
) xs).2;

main = \xs: CList. spec (repr xs);
