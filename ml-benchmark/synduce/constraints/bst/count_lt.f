Inductive Tree = leaf Int | node {Int, Tree, Tree};

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if < a b then b else a;

tmin = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> w
  | node {w, l, r} -> min w (min (f l) (f r))
  end
);

tmax = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> w
  | node {w, l, r} -> max w (max (f l) (f r))
  end
);

is_bst = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    leaf w -> true
  | node {w, l, r} -> and (and (>= w (tmax l)) (<= w (tmin r))) (and (f l) (f r))
  end
);

@Input w: Int;

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf x -> if < x w then 1 else 0
  | node {a, l, r} ->
    + (if < a w then 1 else 0) (+ (f l) (f r))
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf x -> leaf x
  | node {a, l, r} ->
    if < a w then node {a, f l, f r} else node {a, f l, r} /*Avoid the recursion of r*/
  end
);

main = \t: Tree. if is_bst t then spec (target t) else 0;