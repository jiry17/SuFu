Inductive List = nil Unit | cons {Int, List};

snoc = \w: Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> cons {w, nil unit}
  | cons {h, t} -> cons {h, f t}
  end
);

rev1 = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    nil _ -> align (label xs)
  | cons {h, t} ->
    let res = f t in align (label (snoc h (unlabel res)))
  end
);

rev2 = fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} -> snoc h (f t)
  end
);

main = \xs: List.
  let res = rev1 xs in
  align (rev2 (unlabel res));