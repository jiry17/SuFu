Config NonLinear = true;

Inductive List = nil Unit | cons {Int, List};

div2 = \x: Int. / x 2;

from = \a: Int. \b: Int. (fix (
  \f: Int -> Int -> Compress List. \now: Int. \r: Int.
  if > now r then nil unit
  else cons {now, f (+ now 1) r}
)) a b;

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

main = \a: Int. \b: Int. if <= a b then sum (from a b) else 0;