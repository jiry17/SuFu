Config SampleSize = 20;

Inductive Tree = leaf Int | node {Tree, Tree};
Inductive List = nil Unit | cons {Int, List};

cat = fix (
  \f: List -> List -> List. \a: List. \b: List.
  match a with
    nil _ -> b
  | cons {h, t} -> cons {h, (f t b)}
  end
);

max = \a: Int. \b: Int. if < a b then b else a;

depth = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> 0
  | node {l, r} -> + 1 (max (f l) (f r))
  end
);

deepest = fix (
  \f: Tree -> {List, Compress Tree}. \t: Tree.
  match t with
    leaf w -> {cons {w, nil unit}, t}
  | node {l, r} ->
    let lres = f l in
      let rres = f r in
        if > (depth lres.2) (depth rres.2) then
          {lres.1, t}
        else if == (depth lres.2) (depth rres.2) then
          {cat lres.1 rres.1, t}
        else {rres.1, t}
  end
);

main = \t: Tree. (deepest t).1;