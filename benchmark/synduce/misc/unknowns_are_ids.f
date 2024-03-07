Inductive List = nil Unit | cons {Int, List};
Inductive CList = cnil Unit | single Int | concat {CList, CList};

spec = fix (
  \f: List -> Int. \x: List.
  match x with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

cat = fix (
  \f: List -> List -> List. \a: List. \b: List.
  match a with
    nil _ -> b
  | cons {h, t} -> cons {h, (f t b)}
  end
);

repr = fix (
    \f: CList -> Compress List. \xs: CList.
    match xs with
      cnil _ -> nil unit
    | single x -> cons {x, nil unit}
    | concat {a, b} -> cat (f a) (f b)
    end
);

main = \x: CList. spec (repr x);