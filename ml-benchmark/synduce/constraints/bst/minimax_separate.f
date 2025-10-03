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


spec = fix (
  \f: Tree -> {Int, Int}. \t: Tree.
  match t with
    leaf x -> {x, x}
  | node {a, l, r} ->
    let lres = f l in let rres = f r in
      {max a (max lres.1 rres.1), min a (min lres.2 rres.2)}
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf x -> leaf x
  | node {a, l, r} ->
    /*These invocations are provided in Synduce's template*/
    let lmin = tmin l in let lmax = tmax l in
    let rmin = tmin r in let rmax = tmax r in
      node {a, l, r} /*Avoid recursions*/
  end
);

main = \t: Tree. if is_bst t then spec (target t) else {0, 0};