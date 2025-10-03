Inductive List = two {Int, Int} | cons {Int, List};
Inductive CList = ctwo {Int, Int} | concat {CList, CList};

max = \a: Int. \b: Int. if < a b then b else a;
min = \a: Int. \b: Int. if > a b then b else a;

cat_list = fix (
  \f: List -> List -> List. \xs: List. \ys: List.
  match xs with
    two {a, b} -> cons {a, cons {b, ys}}
  | cons {hd, tl} -> cons {hd, f tl ys}
  end
);

repr = fix (
  \f: CList -> Compress List. \xs: CList.
  match xs with
    ctwo {a, b} -> two {a, b}
  | concat {a, b} -> cat_list (f a) (f b)
  end
);

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    two {a, b} -> {min a b, max a b}
  | cons {hd, tl} ->
    let result = f tl in
    let new_min = min hd result.1 in
    let new_snd = min result.2 (max hd result.1) in
    {new_min, new_snd}
    end
) xs).2;

main = \x: CList. spec (repr x);