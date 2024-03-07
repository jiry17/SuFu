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

@Input winp: Int;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt a -> if < a winp then 1 else 0
  | cons {h, t} ->
    + (if < h winp then 1 else 0) (f t)
  end
);

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt w -> xs
  | cons {h, t} ->
    if < h winp then cons {h, f t}
    else xs /*Avoid recursions*/
  end
);

main = \xs: List. if is_sorted xs then spec (target xs) else 0;