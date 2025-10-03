Inductive List = nil Unit | cons {Int, List};

map = \f: Int -> Int. fix (
  \g: List -> List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> cons {f h, g t}
  end
);

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

square = \x: Int. * x x;

upto = \n: Int. (fix (
  \f: Int -> Compress List. \now: Int.
  if > now n then nil unit
  else cons {now, f (+ now 1)}
)) 1;

/*sqsum = \n: Int. / (* n (* (+ n 1) (+ n (+ n 1)))) 6;*/

main = \n: Int. sum (map square (upto n));