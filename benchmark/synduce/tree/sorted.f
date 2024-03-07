Inductive Tree = leaf Int | node {Int, Tree, Tree};

spec = \t: Tree. (fix (
  \f: Tree -> {Int, Bool}. \t: Tree.
  match t with
    leaf a -> {a, true}
  | node {a, l, r} ->
    let r1 = (f l) in
    let r2 = (f r) in
    {a, and (and (and (< r1.1 a) (< a r2.1)) r1.2) r2.2}
  end
) t).2;

repr = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf a -> leaf a
  | node {a, l, r} -> node {a, f l, f r}
  end
);

main = \t: Tree. spec (repr t);
