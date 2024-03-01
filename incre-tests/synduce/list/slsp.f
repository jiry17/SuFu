Inductive List = elt Int | cons {Int, List};
Inductive CnList = sglt Int | cat {CnList, Int, CnList};

max = \x: Int. \y: Int. if (> x y) then x else y;

cat_list = fix (
  \f: List -> List -> List. \xs: List. \ys: List.
  match xs with
    elt a -> cons {a, ys}
  | cons {hd, tl} -> cons {hd, f tl ys}
  end
);

repr = fix (
  \f: CnList -> Compress List. \xs: CnList.
  match xs with
    sglt a -> elt a
  | cat {a, b, c} -> cat_list (f a) (f c)
  end
);

spec = \xs: List. (fix (
  \f: List -> {Int, Bool}. \xs: List.
  match xs with
    elt a -> {max 0 a, (>= a 0)}
  | cons {hd, tl} ->
    let result = f tl in
    let new_cond = and result.2 (>= hd 0) in
    {if new_cond then + hd result.1 else result.1, new_cond}
  end
) xs).1;

main = \x: CnList. spec (repr x);