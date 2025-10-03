Inductive Tree = nil Unit | node {Int, Tree, Tree};

max = \a: Int. \b: Int. if < a b then b else a;

height = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    nil _ -> 0
  | node {w, l, r} -> + 1 (max (f l) (f r))
  end
);

balanced = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    nil _ -> true
  | node {w, l, r} -> and (and (== (height l) (height r)) (f l)) (f r)
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    nil _ -> nil unit
  | node {w, l, r} -> node {w, f l, f r}
  end
);

main = \t: Tree. if balanced t then height (target t) else 0;