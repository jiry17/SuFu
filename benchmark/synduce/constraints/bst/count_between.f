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

@Input lo: Int;
@Input hi: Int;

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf x -> if and (> x lo) (< x hi) then 1 else 0
  | node {a, l, r} ->
    + (if and (> a lo) (< a hi) then 1 else 0) (+ (f l) (f r))
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf x -> leaf x
  | node {a, l, r} ->
    if <= a lo then node {a, l, f r} /*Avoid the recursion of l*/
    else if >= a hi then node {a, f l, r} /*Avoid the recursion of r*/
    else node {a, f l, f r}
  end
);

main = \t: Tree. if is_bst t then spec (target t) else 0;