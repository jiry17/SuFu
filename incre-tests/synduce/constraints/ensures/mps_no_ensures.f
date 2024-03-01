Config ExtraGrammar = "AutoLifter";
Inductive CList = cnil Unit | single Int | concat {CList, CList};
Inductive List = nil Unit | cons {Int, List};

cat = fix (\f: List->List->List. \x: List. \y: List.
  match x with
    cons {h, t} -> cons {h, f t y}
  | nil _ -> y
  end
);

repr = fix (
  \f: CList -> Compress List. \cl: CList.
  match cl with
    cnil _ -> nil unit
  | single h -> cons {h, nil unit}
  | concat {l, r} -> cat (f l) (f r)
  end
);

max = \a: Int. \b: Int. if < a b then b else a;

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    nil _ -> {0, 0}
  | cons {h, t} ->
    let res = f t in
      {max 0 (+ res.1 h), + res.2 h}
  end
) xs).1;

@Start main = \cl: CList. spec (repr cl);