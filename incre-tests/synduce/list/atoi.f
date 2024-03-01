Config NonLinear = true;
Config SampleSize = 6;
Config ExtraGrammar = "AutoLifter";

Inductive List = nil Unit | cons {Int, List};
Inductive CList = cnil Unit | single Int | concat {CList, CList};

base = 10;

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \x: List.
  match x with
    nil _ -> {0, 1}
  | cons {h, t} ->
    let result  = (f t) in
    {+ h (* 10 result.1), * 10 result.2}
  end
) xs).1;

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