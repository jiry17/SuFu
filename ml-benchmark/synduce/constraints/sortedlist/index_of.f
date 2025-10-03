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

@Input key: Int;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt w -> if == w key then 1 else 0
  | cons {h, t} ->
    let res = f t in
      if == key h then 1 else if == res 0 then 0 else + 1 res
  end
);

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt w -> xs
  | cons {h, t} ->
    if >= h key then xs /*Avoid recursions*/
    else cons {h, f t}
  end
);

main = \xs: List. if is_sorted xs then spec (target xs) else 0;