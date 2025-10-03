Inductive List = elt Int | cons {Int, List};
Inductive NList = line List | ncons {List, NList};

max = \a: Int. \b: Int. if < a b then b else a;
lmax = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> x
  | cons {h, t} -> max h (f t)
  end
);

min = \a: Int. \b: Int. if < a b then a else b;
lmin = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> x
  | cons {h, t} -> min h (f t)
  end
);

is_sorted =
  let aux = fix (
    \f: Int -> NList -> Bool. \pre: Int. \xs: NList.
    match xs with
      line x -> <= pre (lmax x)
    | ncons {h, t} -> and (<= pre (lmax h)) (f (lmax h) t)
    end
  ) in \xs: NList.
  match xs with
    line _ -> true
  | ncons {h, t} -> aux (lmax h) t
  end;

interval = fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    elt x -> {x, x}
  | cons {h, t} ->
    let res = f t in
      {min res.1 h, max res.2 h}
  end
);

spec = \xs: NList. (fix (
  \f: NList -> {Int, Int, Bool}. \xs: NList.
  match xs with
    line x ->
      let res = interval x in
        {res.1, res.2, true}
  | ncons {h, t} ->
      let info = interval h in
        let res = f t in
          {min info.1 res.1, max info.2 res.2,
           and res.3 (and (<= res.1 info.1) (>= res.2 info.2))}
  end
) xs).3;

target = fix (
  \f: NList -> Compress NList. \xs: NList.
  match xs with
    line x ->
      let info = interval x in /*This invocation is provided in Synduce's template*/
        xs
  | ncons {h, t} ->
      let mi = lmin h in /*This invocation is provided in Synduce's template*/
        ncons {h, f t}
   end
);

main = \xs: NList. if is_sorted xs then spec (target xs) else false;
