Inductive List = elt Int | cons {Int, List};

is_sorted =
  let aux = fix (
    \f: Int -> List -> Bool. \pre: Int. \xs: List.
    match xs with
      elt x -> <= pre x
    | cons {h, t} -> and (<= pre h) (f h t)
    end
  ) in \xs: List.
  match xs with
    elt x -> true
  | cons {h, t} -> aux h t
  end;

min = \a: Int. \b: Int. if > a b then b else a;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt w -> w
  | cons {h, t} -> min h (f t)
  end
);

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt w -> xs
  | cons {h, _} -> xs /*Avoid recursions*/
  end
);

main = \xs: List. if is_sorted xs then spec (target xs) else 0;