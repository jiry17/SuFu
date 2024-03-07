Inductive Tree = nil Unit | node {Int, Tree, Tree};

max = \a: Int. \b: Int. if < a b then b else a;

spec = (fix (
  \f: Int -> Tree -> Int. \w: Int. \t: Tree.
  match t with
    nil _ -> w
  | node {a, l, r} -> max a (f (f w r) l)
  end
)) 0;

repr = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    nil _ -> nil unit
  | node {a, l, r} -> node {a, f l, f r}
  end
);

main = \t: Tree. spec (repr t);
