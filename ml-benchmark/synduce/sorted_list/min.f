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

spec = \xs: List.
  match xs with
    elt x -> x
  | cons {h, t} -> h
  end;

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt x -> elt x
  | cons {h, t} -> cons {h, f t}
  end
);

main = \xs: List. spec (sort (target xs));