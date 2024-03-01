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

is_pos = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    elt x -> > x 0
  | cons {h, t} -> and (> h 0) (f t)
  end
);

max = \x: Int. \y: Int. if < x y then y else x;
min = \x: Int. \y: Int. if > x y then y else x;

spec = \xs: List. (fix (
  \f: List -> {Int, Int, Int}. \xs: List.
  match xs with
    elt x -> {0, x, x}
  | cons {h, t} ->
    let res = f t in
      let newmax = max h res.2 in
        let newmin = min h res.3 in
          {max res.1 (- newmax newmin), newmax, newmin}
  end
) xs).1;

last = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt a -> a
  | cons {h, t} -> f t
  end
);

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt w -> xs
  | cons {h, t} ->
    let aux = last t in /*This invocation is provided in Synduce's template*/
      cons {h, t} /*Avoid recursions*/
  end
);

main = \xs: List. if and (is_sorted xs) (is_pos xs) then spec (target xs) else 0;