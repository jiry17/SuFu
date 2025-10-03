Inductive List = elt Int | cons {Int, List};
Inductive Pos = one Unit | s Pos;
Inductive Map = value {Int, Pos} | node {Int, Map, Map};

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
    cons {h, t} -> cons {h, f t y}
  | elt w -> cons {w, y}
  end
);

repeat = \w: Int. fix (
  \f: Pos -> List. \n: Pos.
  match n with
    one _ -> elt w
  | s n' -> cons {w, f n'}
  end
);

repr = fix (
  \f: Map -> List. \m: Map.
  match m with
    value {k, v} -> repeat k v
  | node {a, l, r} -> cat (f l) (f r)
  end
);

count = \w: Int. fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt h -> if == h w then 1 else 0
  | cons {h, t} -> + (if == h w then 1 else 0) (f t)
  end
);

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \l: List.
  match l with
    elt v -> {1, v}
  | cons {h, t} ->
    let cnt = count h l in
      let res = f t in
        if > cnt res.1 then {cnt, h} else res
  end
) xs).2;

p2i = fix (
  \f: Pos -> Int. \n: Pos.
  match n with
    one _ -> 1
  | s m -> + 1 (f m)
  end
);

target = fix (
  \f: Map -> Compress Map. \m: Map.
  match m with
    value {k, v} ->
      /* This invocation is provided in Synduce's template */
      let cnt = p2i v in
        value {k, v}
  | node {a, l, r} -> node {a, f l, f r}
  end
);

main = \m: Map. if is_map m then spec (repr (target m)) else 0;