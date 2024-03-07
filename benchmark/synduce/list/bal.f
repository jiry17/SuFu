Config ExtraGrammar = "AutoLifter";
Inductive List = nil Unit | cons {Int, List};
Inductive CList = cnil Unit | single Int | concat {CList, CList};

min = \a: Int. \b: Int. if > a b then b else a;

spec = \xs: List. (fix (
  \f: List -> {Int, Int, Bool}. \x: List.
  match x with
    nil _ -> {0, 0, true}
  | cons {h, t} ->
    let result = f t in
    let cnt = result.1 in
    let min_cnt = result.2 in
    let bal = result.3 in
    let new_cnt = if (> h 0) then (+ cnt 1) else (- cnt 1) in
    {new_cnt, min min_cnt new_cnt, and bal (>= new_cnt 0)}
  end
) xs).3;

cat = fix (
  \f: List -> List -> List. \a: List. \b: List.
  match a with
    nil _ -> b
  | cons {h, t} -> cons {h, (f t b)}
  end
);

repr = fix (
    \f: CList -> Compress List. \xs: CList.
    match xs with
      cnil _ -> nil unit
    | single x -> cons {x, nil unit}
    | concat {a, b} -> cat (f a) (f b)
    end
);

main = \x: CList. spec (repr x);