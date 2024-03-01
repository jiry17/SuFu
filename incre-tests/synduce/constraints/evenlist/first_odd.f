Inductive List = elt Int | cons {Int, List};

mod2 = \x: Int. - x (* (/ x 2) 2);

is_even_pos = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    elt x -> and (> x 0) (== (mod2 x) 0)
  | cons {h, t} -> and (and (> h 0) (== (mod2 h) 0)) (f t)
  end
);

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> if == (mod2 x) 1 then x else 0
  | cons {h, t} -> if == (mod2 h) 1 then h else f t
  end
);

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt x -> elt x
  | cons {h, t} -> cons {h, t} /*Avoid recursions*/
  end
);

main = \xs: List. if is_even_pos xs then spec (target xs) else 0;
