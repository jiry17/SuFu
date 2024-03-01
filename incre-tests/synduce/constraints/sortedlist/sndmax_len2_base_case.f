Config ExtraGrammar = "AutoLifter";
Inductive List = nil Unit | cons {Int, Int, List};
Inductive SList = snil Unit | scons {Int, SList};

repr = fix (
  \f: List -> SList. \xs: List.
  match xs with
    nil _ -> snil unit
  | cons {a, b, t} -> scons {a, scons {b, f t}}
  end
);

next_is_lt = \pre: Int. \xs: List.
  match xs with
    nil _ -> true
  | cons {a, b, t} -> > pre a
  end;

is_sorted = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    nil _ -> true
  | cons {a, b, t} -> and (and (> a 0) (> b 0))
                          (and (> a b) (and (next_is_lt b t) (f t)))
  end
);

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if < a b then b else a;

spec = \xs: SList. (fix (
  \f: SList -> {Int, Int}. \xs: SList.
  match xs with
    snil _ -> {0, 0}
  | scons {h, t} ->
    let res = f t in
      {max h res.1, max res.2 (min h res.1)}
  end
) xs).2;

target = fix (
  \f: List -> Compress List. \c: List.
  match c with
    nil _ -> c
  | cons {a, b, t} -> c /*Avoid recursion*/
  end
);

main = \xs: List.
  if is_sorted xs then spec (repr (target xs)) else 0;