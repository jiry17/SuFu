Inductive Tree = nil Unit | node {Int, Tree, Tree};

max = \a: Int. \b: Int. if < a b then b else a;

spec = \t: Tree. (fix (
  \f: {Int, Int} -> Tree -> {Int, Int}. \s: {Int, Int}. \t: Tree.
  match t with
    nil _ -> s
  | node {a, l, r} ->
    let result = (f s l) in
    f {+ a result.1, max result.2 (+ a result.1)} r
  end
) {0,0} t).2;

repr = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    nil _ -> nil unit
  | node {a, l, r} -> node {a, f l, f r}
  end
);

main = \t: Tree. spec (repr t);
