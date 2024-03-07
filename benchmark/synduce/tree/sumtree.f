Inductive Tree = nil Unit | node {Int, Tree, Tree};

spec = (fix (
  \f: Int -> Tree -> Int. \w: Int. \t: Tree.
  match t with
    nil _ -> w
  | node {a, l, r} -> f (+ (f w l) a) r
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
