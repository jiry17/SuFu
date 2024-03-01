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

SearchUnit = {Int, List};

spec = \p: TwoList. (fix (
  \f: List -> Bool. \xs: List.
  match xs with
    elt a -> find a p.1
  | cons {h, t} -> or (find h p.1) (f t)
  end
)) p.2;

target =
  let aux = fix (
    \f: Int -> List -> Compress {Int, List}. \w: Int. \xs: List.
    match xs with
      elt b -> {w, xs}
    | cons {h, t} ->
      if > w h then {w, xs} /*Avoid recursions*/
      else let res = f w t in
        {w, cons {h, res.2}}
    end
  ) in fix (
    \f: TwoList -> Compress TwoList. \p: TwoList.
    match p.1 with
      elt a -> let res = aux a p.2 in
        {elt a, res.2}
    | cons {h, t} -> let res1 = f {t, p.2} in
      let res2 = aux h p.2 in
        {cons {h, res1.1}, res1.2}
    end
  );

main = \p: TwoList. if is_sorted_pair p then spec (target p) else false;