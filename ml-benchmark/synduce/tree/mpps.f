Inductive Tree = nil Unit | single Int | node {Int, Tree, Tree};

max = \a: Int. \b: Int. if < a b then b else a;

spec = \t: Tree. (fix (
  \f: {Int, Int} -> Tree -> {Int, Int}. \s: {Int, Int}. \t: Tree.
  match t with
    nil _ -> s
  | single a -> {+ s.1 a, max s.2 (+ s.1 a)}
  | node {a, l, r} -> f (f {+ s.1 a, max s.2 (+ s.1 a)} l) r
  end
) {0,0} t).2;

repr = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    nil _ -> nil unit
  | single a -> single a
  | node {a, l, r} -> node {a, f l, f r}
  end
);

main = \t: Tree. spec (repr t);
