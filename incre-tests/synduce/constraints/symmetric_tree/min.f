Inductive Tree = leaf Int | node {Int, Tree, Tree};

is_sym_pair = fix (
  \f: Tree -> Tree -> Bool. \l: Tree. \r: Tree.
  match {l, r} with
    {leaf x1, leaf x2} -> == x1 x2
  | {node {x1, l1, r1}, node {x2, l2, r2}} -> and (== x1 x2) (and (f r1 l2) (f l1 r1))
  | _ -> false
  end
);

is_sym = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    leaf _ -> true
  | node {_, l, r} -> and (is_sym_pair l r) (and (f l) (f r))
  end
);

min = \a: Int. \b: Int. if < a b then a else b;

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> w
  | node {w, l, r} -> min w (min (f l) (f r))
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf w -> leaf w
  | node {w, l, r} -> node {w, f l, r} /*Avoid the recursion of r*/
  end
);

main = \t: Tree.
  if is_sym t then spec (target t) else 0;