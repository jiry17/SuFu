Inductive SList = elt Int | cons {Int, SList};
Inductive CList = single Int | concat {CList, CList};

cat_list = fix (
  \f: SList -> SList -> SList. \xs: SList. \ys: SList.
  match xs with
    elt a -> cons {a, ys}
  | cons {a, b} -> cons {a, f b ys}
  end
);

repr = fix (
  \f: CList -> Compress SList. \xs: CList.
  match xs with
    single a -> elt a
  | concat {a, b} -> cat_list (f a) (f b)
  end
);

spec = \xs: SList. (fix (
  \f: SList -> {Int, Int, Bool}. \xs: SList.
  match xs with
    elt a -> {a, a, true}
  | cons {hd, tl} ->
    let result = f tl in
    {hd, result.2, and result.3 (< hd result.1)}
  end
) xs).3;

main = \xs: CList. spec (repr xs);
