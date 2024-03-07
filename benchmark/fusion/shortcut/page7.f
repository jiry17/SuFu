Inductive List = nil Unit | cons {Int, List};

div = \x: Int. \y: Int. if == y 0 then 0 else / x y;

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

length = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

res = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> cons {h, f t}
  end
);

main = \xs: List.
  let oup = res xs in
    div (sum oup) (length oup);