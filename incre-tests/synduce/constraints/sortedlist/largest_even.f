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

max = \x: Int. \y: Int. if < x y then y else x;
mod2 = \x: Int. - x (* 2 (/ x 2));

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> x
  | cons {h, t} ->
    if == 0 (mod2 h) then max h (f t) else f t
  end
);

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt w -> xs
  | cons {h, t} ->
    if == 0 (mod2 h) then xs /*Avoid recursion*/
    else cons {h, f t}
  end
);

main = \xs: List. if is_sorted xs then spec (target xs) else 0;