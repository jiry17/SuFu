Inductive CList = empty Unit | elt Int | concat {CList, CList};
Inductive List = nil Unit | cons {Int, List};

cat = fix (
  \f: List -> List -> List. \a: List. \b: List.
  match a with
    nil _ -> b
  | cons {h, t} -> cons {h, (f t b)}
  end
);

repr = fix (
  \f: CList -> List. \xs: CList.
  match xs with
    empty _ -> nil unit
  | elt w -> cons {w, nil unit}
  | concat {l, r} -> cat (f l) (f r)
  end
);

all_pos = fix (
  \f: CList -> Bool. \c: CList.
  match c with
    empty _ -> true
  | elt w -> > w 0
  | concat {l, r} -> and (f l) (f r)
  end
);

geq_head = \x: Int. \xs: List.
  match xs with
    nil _ -> true
  | cons {h, t} -> >= x h
  end;

is_sorted = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    nil _ -> true
  | cons {h, t} -> and (geq_head h t) (f t)
  end
);

min = \x: Int. \y: Int. if < x y then x else y;
max = \x: Int. \y: Int. if > x y then x else y;

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    nil _ -> {0, 0}
  | cons {h, t} ->
    let res = f t in
      {max res.1 h, max res.2 (min res.1 h)}
  end
) xs).2;

target = fix (
  \f: CList -> Compress CList. \c: CList.
  match c with
    empty _ -> c
  | elt w -> c
  | concat {l, r} -> concat {f l, f r}
  end
);

main = \c: CList.
  if is_sorted (repr c) then spec (repr (target c)) else 0;