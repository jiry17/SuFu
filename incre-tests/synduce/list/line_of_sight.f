Inductive List = single Int | cons {Int, List};
Inductive CList = elt Int | cat {CList, CList};

max = \a: Int. \b: Int. if < a b then b else a;

spec = \xs: List. (fix (
  \f: List -> {Int, Int, Bool}. \xs: List.
  match xs with
    single a -> {a, a, true}
  | cons {hd, tl} -> 
    let result = f tl in
    {hd, max result.2 hd, > hd result.2}
  end
) xs).3;

cat_list = fix (
  \f: List -> List -> List. \xs: List. \ys: List.
  match xs with
    single a -> cons {a, ys}
  | cons {a, b} -> cons {a, f b ys}
  end
);

repr = fix (
  \f: CList -> Compress List. \xs: CList.
  match xs with
    elt a -> single a
  | cat {a, b} -> cat_list (f a) (f b)
  end
);

main = \x: CList. spec (repr x);