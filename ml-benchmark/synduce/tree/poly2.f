Config NonLinear = true;

Inductive Tree = nil Unit | node {Int, Tree, Tree};

max = \a: Int. \b: Int. if < a b then b else a;

@Input x: Int;

spec = \xs: Tree. (fix (
  \f: {Int, Int} -> Tree -> {Int, Int}. \s: {Int, Int}. \t: Tree.
  match t with
    nil _ -> s
  | node {a, l, r} ->
    let result = (f s l) in
    f {+ a (* x result.1), * x result.2} r
  end
) {0,1} xs).1;

repr = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    nil _ -> nil unit
  | node {a, l, r} -> node {a, f l, f r}
  end
);

main = \t: Tree. spec (repr t);
