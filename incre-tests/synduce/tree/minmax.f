Inductive Tree = leaf Int | node {Int, Tree, Tree};

max = \a: Int. \b: Int. if < a b then b else a;
min = \a: Int. \b: Int. if > a b then b else a;

g = fix (
  \f: {Int, Int} -> Tree -> {Int, Int}. \x: {Int, Int}. \t: Tree.
  match t with
    leaf a -> {min a x.1, max a x.2}
  | node {a, l, r} -> f (f {min a x.1, max a x.2} l) r
  end
);

spec = fix (
  \f: Tree -> {Int, Int}. \t: Tree.
  match t with
    leaf x -> {x,x}
  | node {a, l, r} ->
    let result = g {a, a} l in
    g {result.1, result.2} r
  end
);

repr = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf a -> leaf a
  | node {a, l, r} -> node {a, f l, f r}
  end
);

main = \t: Tree. spec (repr t);
