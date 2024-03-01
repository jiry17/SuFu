Inductive List = elt Int | cons {Int, List};

insert = \y: Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    elt x -> if < y x then cons {y, elt x} else cons {x, elt y}
  | cons {h, t} -> if < y h then cons {y, xs} else cons {h, f t}
  end
);

sort = fix (
  \f: List -> List. \xs: List.
  match xs with
    elt x -> elt x
  | cons {h, t} -> insert h (f t)
  end
);

len = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt _ -> 1
  | cons {_, t} -> + 1 (f t)
  end
);

is_length_gt2 = \xs: List. >= (len xs) 2;

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt x -> elt x
  | cons {h, t} -> cons {h, f t}
  end
);

main = \xs: List. if is_length_gt2 xs then len (sort (target xs)) else 0;