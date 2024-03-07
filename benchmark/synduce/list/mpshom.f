Config ExtraGrammar = "AutoLifter";
Inductive List = nil Unit | cons {Int, List};
Inductive CList = cnil Unit | single Int | concat {CList, CList};

max = \a: Int. \b: Int. if < a b then b else a;

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    nil _ -> {0, 0}
  | cons {hd, tl} ->
    let result = f tl in
    let new_mps = max 0 (+ hd result.1) in
    let new_sum = + hd result.2 in
    {new_mps, new_sum}
  end
) xs).1;

cat = fix (
  \f: List -> List -> List. \xs: List. \ys: List.
  match xs with
    nil _ -> ys
  | cons {a, b} -> cons {a, f b ys}
  end
);

repr = fix (
  \f: CList -> Compress List. \xs: CList.
  match xs with
    cnil _ -> nil unit
  | single a -> cons {a, nil unit}
  | concat {a, b} -> cat (f a) (f b)
  end
);

main = \x: CList. spec (repr x);