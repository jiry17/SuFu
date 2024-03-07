Inductive List = nil Unit | cons {Int, List};
Inductive IndexedList = inil Unit | icons {Int, Int, IndexedList};

length = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

@Input w: Int;

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
  | icons {h, i, t} -> if == h w then i else f t
  end
);

main = \xs: List. spec (repr xs);