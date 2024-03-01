Inductive List = nil Unit | cons {Int, List};

max = \a: Int. \b: Int. if < a b then b else a;

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

mts = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> max (+ h (sum t)) (f t)
  end
);

spec = \xs: List. mts xs;

repr = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> cons {h, f t}
  end
);

main = \xs: List. spec (repr xs);