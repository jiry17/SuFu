Config ExtraGrammar = "AutoLifter";
Config NonLinear = true;

Inductive List = nil Unit | cons {Int, List};
Inductive IndexedList = inil Unit | icons {Int, Int, IndexedList};

length = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

repr = fix (
  \f: List -> Compress IndexedList. \xs: List.
  match xs with
    nil _ -> inil unit
  | cons {h, t} -> icons {h, length t, f t}
  end
);

spec = fix (
  \f: IndexedList -> Int. \xs: IndexedList.
  match xs with
    inil _ -> 0
  | icons {h, i, t} -> + (* h i) (f t)
  end
);

main = \xs: List. spec (repr xs);