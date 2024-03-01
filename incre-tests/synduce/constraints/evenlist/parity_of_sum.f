Inductive List = elt Int | cons {Int, List};

mod2 = \x: Int. - x (* (/ x 2) 2);

is_even = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    elt x -> and (> x 0) (== (mod2 x) 0)
  | cons {h, t} -> and (and (> h 0) (== (mod2 h) 0)) (f t)
  end
);

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> mod2 x
  | cons {h, t} -> + (mod2 h) (f t)
  end
);

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt x -> elt x
  | cons {h, t} -> cons {h, t} /*Avoid recursions*/
  end
);

main = \xs: List. if is_even xs then spec (target xs) else 0;
