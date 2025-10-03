Inductive List = elt Int | cons {Int, List};
Inductive CList = single Int | concat {CList, CList};

max = \x: Int. \y: Int. if (> x y) then x else y;

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    elt a -> {a, 1}
  | cons {hd, tl} -> 
    let result = f tl in
    let new_max = max result.1 hd in
    let new_cnt = if (> hd result.1) then 1 else + result.2 (if (== hd result.1) then 1 else 0) in
    {new_max, new_cnt}
  end
) xs).2;

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