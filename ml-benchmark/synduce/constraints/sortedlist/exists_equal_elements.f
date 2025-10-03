Inductive List = elt Int | cons {Int, List};
TwoList = {List, List};

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

is_sorted_pair =
  \p: TwoList. and (is_sorted p.1) (is_sorted p.2);

find = \w: Int. fix (
  \f: List -> Bool. \xs: List.
  match xs with
    elt x -> == x w
  | cons {h, t} -> or (== w h) (f t)
  end
);

spec = \p: TwoList. (fix (
  \f: List -> Bool. \xs: List.
  match xs with
    elt a -> find a p.1
  | cons {h, t} -> or (find h p.1) (f t)
  end
)) p.2;

target = fix (
  \f: TwoList -> Compress TwoList. \p: TwoList.
  match p with
    {elt a, elt b} -> p
  | {elt a, cons {h, t}} ->
    if > a h then p /*Avoid recursions*/ else
      let res = f {elt a, t} in
        {res.1, cons {h, res.2}}
  | {cons {h, t}, elt b} ->
    if > b h then p /*Avoid recursions*/ else
      let res = f {t, elt b} in
        {cons {h, res.1}, res.2}
  | {cons {h1, t1}, cons {h2, t2}} ->
    if > h1 h2 then
      let res = f {t1, p.2} in
        {cons {h1, res.1}, res.2}
    else
      let res = f {p.1, t2} in
        {res.1, cons {h2, res.2}}
  end
);

main = \p: TwoList. if is_sorted_pair p then spec (target p) else false;