Config NonLinear = true;
Config SampleIntMax = 3;
Config SampleIntMin = -3;
Config SampleSize = 6;

Inductive Tree = nil Unit | node {Int, Tree, Tree};

@Input x: Int;

spec = \t: Tree. (fix (
  \f: {Int, Int} -> Tree -> {Int, Int}. \s: {Int, Int}. \t: Tree.
  match t with
    nil _ -> s
  | node {a, l, r} ->
    let result = (f s l) in
    f {+ result.1 (* result.2 a), * result.2 x} r
  end
) {0,1} t).1;

repr = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    nil _ -> nil unit
  | node {a, l, r} ->
    node {a, f l, f r}
  end
);

main = \t: Tree. spec (repr t);
