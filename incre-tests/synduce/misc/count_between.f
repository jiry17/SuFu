Inductive Tree = leaf Int | node {Int, Tree, Tree};

max = \a: Int. \b: Int. if < a b then b else a;
min = \a: Int. \b: Int. if < a b then a else b;

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

@Input lo: Int;
@Input hi: Int;

repr = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf w -> leaf w
  | node {w, l, r} ->
    if (>= w hi) then node {w, f l, f r} else node {w, f l, f r}
  end
);

spec = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    leaf w -> (and (< lo w) (< w hi))
  | node {w, l, r} -> or (or (and (< lo w) (< w hi)) (f l)) (f r)
  end
);

main = \t: Tree. if < lo hi then spec (repr t) else false;
