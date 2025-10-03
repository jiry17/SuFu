Inductive List = elt {Int, Int} | cons {Int, Int, List};
Inductive Map = value {Int, Int} | node {Int, Map, Map};

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if > a b then a else b;

min_key = fix (
  \f: Map -> Int. \m: Map.
  match m with
    value {k, v} -> k
  | node {a, l, r} -> min (f l) (f r)
  end
);

max_key = fix (
  \f: Map -> Int. \m: Map.
  match m with
    value {k, v} -> k
  | node {a, l, r} -> max (f l) (f r)
  end
);

is_map = fix (
  \f: Map -> Bool. \m: Map.
  match m with
    value {k, v} -> true
  | node {a, l, r} -> and (and (< (max_key l) a) (<= a (min_key r))) (and (f l) (f r))
  end
);

cat = fix (\f: List->List->List. \x: List. \y: List.
  match x with
    cons {k, v, t} -> cons {k, v, f t y}
  | elt {k, v} -> cons {k, v, y}
  end
);

repr = fix (
  \f: Map -> List. \m: Map.
  match m with
    value {k, v} -> elt {k, v}
  | node {k, l, r} -> cat (f l) (f r)
  end
);

@Input key: Int;

spec = fix (
  \f: List -> Int. \l: List.
  match l with
    elt {k, v} -> if > k key then v else 0
  | cons {k, v, t} ->
    + (if > k key then v else 0) (f t)
  end
);

target = fix (
  \f: Map -> Compress Map. \m: Map.
  match m with
    value {k, v} -> value {k, v}
  | node {a, l, r} ->
    if > a key then node {a, f l, f r}
    else node {a, l, f r} /*Avoid the recursion of l*/
  end
);

main = \m: Map. if is_map m then spec (repr (target m)) else 0;