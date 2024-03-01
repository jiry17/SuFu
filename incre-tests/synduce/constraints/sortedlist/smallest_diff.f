Inductive List = elt Int | cons {Int, List};

is_sorted =
  let aux = fix (
    \f: Int -> List -> Bool. \pre: Int. \xs: List.
    match xs with
      elt x -> >= pre x
    | cons {h, t} -> and (>= pre h) (f h t)
    end
  ) in \xs: List.
  match xs with
    elt x -> true
  | cons {h, t} -> aux h t
  end;

abs = \x: Int. if < x 0 then - 0 x else x;
min = \x: Int. \y: Int. if < x y then x else y;

min_diff = \w: Int. fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> - 0 (abs (- x w))
  | cons {h, t} -> min (- 0 (abs (- h w))) (f t)
  end
);

spec = fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    elt x -> {0, x}
  | cons {h, t} ->
    {min (min_diff h t) (f t).1, h}
  end
);

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt w -> xs
  | cons {h, t} -> cons {h, f t}
  end
);

main = \xs: List. if is_sorted xs then spec (target xs) else {0, 0};