Inductive List = nil Unit | cons {Int, List};

max = \a: Int. \b: Int. if < a b then b else a;

mts = fix (
  \f: Int -> List -> Int. \pre: Int. \xs: List.
  match xs with
    nil _ -> pre
  | cons {h, t} -> f (max 0 (+ h pre)) t
  end
);

mps = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> max (+ h (f t)) 0
  end
);

spec = \xs: List. {mts 0 xs, mps xs};

repr = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> cons {h, f t}
  end
);

main = \xs: List. spec (repr xs);