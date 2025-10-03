Inductive List = nil Unit | cons {{Int, Int}, List};
Inductive CList = cnil Unit | single {Int, Int} | concat {CList, CList};

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

@Input xinp: Int;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> 
    let key = h.1 in
    let value = h.2 in
    if (== xinp key) then + value (f t) else (f t)
  end
);

main = \x: CList. spec (repr x);