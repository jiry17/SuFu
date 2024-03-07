Inductive ZipCList = cnil Unit| single {Int, Int} | concat {ZipCList, ZipCList};
Inductive ZipList = nil Unit | cons {Int, Int, ZipList};

cat = fix (
  \f: ZipList -> ZipList -> ZipList. \xs: ZipList. \ys: ZipList.
  match xs with
    nil _ -> ys
  | cons {a, b, t} -> cons {a, b, (f t ys)}
  end
);

repr = fix (
  \f: ZipCList -> Compress ZipList. \xs: ZipCList.
  match xs with
    cnil _ -> nil unit
  | single {a, b} -> cons {a, b, nil unit}
  | concat {a, b} -> cat (f a) (f b)
  end
);

spec = fix (
  \f: ZipList -> Int. \xs: ZipList.
  match xs with
    nil _ -> 0
  | cons {a, b, t} -> if (== a b) then + 1 (f t) else (f t)
  end
);

main = \xs: ZipCList. spec (repr xs);